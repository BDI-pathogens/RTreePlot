# private function to calculate the position of the nodes in the tree
#
# t ia a data.table with columns parent, child and time
#
.tree_calculate_node_positions = function( t )
{
  # traverse down to set on levels
  roots = t[ child == parent ][ , .( child, parent, time,level = 1  )]
  last  = roots[ , .( parent = child, p = TRUE)]
  remain = t[ child != parent]
  levels = list()
  ldx    = 1;
  levels[[1]] = roots

  N_last = remain[,.N] + 1
  while( remain[ ,.N ] < N_last )
  {
    N_last = remain[,.N]
    remain = last[ remain , on = "parent"]
    if( remain[!is.na(p),.N]  == 0 )
      break

    ldx = ldx + 1
    levels[[ldx]] = remain[ !is.na( p ),.(parent, child, time,level = ldx)]
    last   = levels[[ldx]][ ,.( parent = child, p = TRUE ) ]

    remain = remain[ is.na( p),.( parent,child, time)]
  }
  ldx_max = ldx

  # traverse up to count the number of descendants
  levels[[ ldx]][ , N := 1]
  levels[[ ldx]][ , N_direct := 0]
  next_level = levels[[ldx]][ , .(parent, N,N_direct )]
  while( ldx > 1 )
  {
    last_level = next_level[, .( child= parent,  N, N_direct ) ]
    ldx = ldx - 1

    next_level = last_level[ levels[[ldx]], on = "child"]
    next_level[ , N := ifelse( is.na(N), 1, N+1  )]
    levels[[ldx]] = next_level

    next_level = next_level[ , .( N = sum(N ), N_direct = .N), by = "parent"]
  }

  # traverse down to calculate the heigh on the plot (between 0 and 1)
  ldx = 1
  N_tot = sum(unlist(lapply(levels, function(t) t[,.N])))
  levels[[1]] = levels[[1]][ order( time)]
  levels[[1]][ , N_cs := cumsum( N )]
  levels[[1]][ , pos := ( N_cs ) / N_tot]

  while( ldx < ldx_max )
  {
    levels[[ldx+1]] = levels[[ldx]][ ,.( parent = child, pos_p = pos, time_p = time, N_p = N ) ][ levels[[ldx+1]], on = "parent" ][ order(pos_p,-N)]
    ldx = ldx + 1
    levels[[ldx]][ , N_cs := cumsum( N)]
    max_cs = levels[[ldx]][ , .(N_cs_max = max(N_cs)), by = "parent"]
    levels[[ldx]] = max_cs[ levels[[ldx]], on = "parent"]
    levels[[ldx]][ , offset := -( N_cs_max - N_cs  ) / N_p ]
    levels[[ldx]][ , pos := offset * N_p / N_tot + pos_p ]
    levels[[ldx]][ , c( "N_p", "N_cs", "N_cs_max", "offset") := NULL ]
  }

  all = rbindlist( levels, use.names = TRUE, fill = TRUE)
  all[ , N_direct := ifelse( is.na(N_direct), 0, N_direct)]
  all =  all[ !is.na(pos_p),.(time_min = min( time ) ), by = "parent"][ all, on = "parent"]

  return( all )
}

# private function to calculate the position of the lines to join the nodes
# in the tree
#
.tree_calculate_lines = function( dt_lines, max_lines = 1000 )
{
  one_line = function( x0,y0,x1,y1 )
  {
    return( list(
      type = "line",
      x0   = x0,
      y0   = y0,
      x1   = x1,
      y1   = y1,
      xref = "x",
      yref = "y",
      line = list(width = 0.2)
    ) )
  }

  if( dt_lines[,.N] > max_lines )
  {
    min_N = dt_lines[order(-N)][max_lines,N]
    used_lines = dt_lines[ N >= min_N]
  } else
    used_lines = copy( dt_lines )

  lines = mapply( one_line, used_lines[,time_p],used_lines[,pos_p],
                  used_lines[,time_min], used_lines[,pos], SIMPLIFY = FALSE)
  horiz = used_lines[ time_min != time]
  horiz = mapply( one_line, horiz[,time_min], horiz[,pos],
                  horiz[,time], horiz[,pos], SIMPLIFY = FALSE)
  return( c( lines, horiz) )
}

###################################################################################/
# treeplot.plot
#
# tree plot
###################################################################################/
treeplot.plot = function(
  parent_ids,
  child_ids,
  times,
  max_lines = 1000,
  interactive = TRUE,
  show = TRUE,
  height = 1000
)
{
  if( !is.vector( parent_ids ) | !is.vector( child_ids ) | !is.vector( times ) )
    stop( "parent_id, child_ids and times must all be vectors" )

  n_nodes = length( parent_ids )
  if( length( child_ids ) != n_nodes | length( child_ids ) != n_nodes )
    stop( "parent_ids, child_ids and n_nodes must all be the same length" )

  # put in data,table
  t = data.table(
    child = child_ids,
    parent = parent_ids,
    time = times
  )

  # calculate the tree positions and initial lines
  all = .tree_calculate_node_positions(t)
  all[ , size :=  pmin(N_direct+1,20) / 10 ]
  all[ , text := sprintf( "ID    = %d\nN_tot = %d\nN_dir = %d\n", child, N, N_direct)]

  dt_lines = all[ !is.na(pos_p)]
  lines = .tree_calculate_lines( dt_lines, max_lines = max_lines )

  # calculate the plot
  if( interactive )
  {
    pl = utils.plotly.scatter(
      all,
      c("time", "pos"),
      shapes = lines,
      scatterType = "scattergl",
      height = height,
      xAxisTitle = "time",
      yAxisTitle = ""
    )

    # add appropriate lines when zooming in
    cb = function( xmin,xmax,ymin,ymax)
    {
      if( is.na( xmin) || is.na(xmax) || is.na( ymin) || is.na(ymax) )
        return()

      used_lines = dt_lines[ time_min <= xmax & time >= xmin & pmax( pos, pos_p ) >= ymin & pmin( pos, pos_p ) <= ymax ]
      lines = .tree_calculate_lines( used_lines, max_lines = max_lines )

      pl$setRV("shapes", lines)
      pl$setRV("data", all[ pos <= ymax & pos >= ymin & time >= xmin & time <= xmax ])
    }
    pl$callBacks$addOnRelayoutCallBack( cb)
    if( show )
      pl$show()
  } else {
    pl = plot_ly(
      all,
      x = ~time,
      y = ~pos,
      type = "scattergl",
      mode = "markers",
      height = height
    ) %>% layout(
      xaxis = list( title = "time" ),
      yaxis = list( titlte = "" ),
      shapes = lines
    )

    if( show )
      show( pl )

  }

  return( pl )
}

