reactiveValues = reactiveValues()

if( !exists( "cacheEnv" ) )
  cacheEnv = new.env()

###################################################################################/
# utils.class
#
# add interfaces to R6 class infrastructure
###################################################################################/
utils.class = function(
  classname = NULL,
  public    = list(),
  private   = list(),
  active    = list(),
  inherit   = NULL,
  interfaces = list(),
  lock_objects = TRUE,
  class      = TRUE,
  portable   = TRUE,
  lock_class = FALSE,
  cloneable  = TRUE,
  parent_env = parent.frame(),
  lock
)
{
  # check to see an inherited class has been created by utils.class
  if( !is.null( inherit ) )
  {
    if( inherit$inherit != "utils.class.parent" )
      stop( "inherited classes must be created by utils.class (i.e. so have inherited utils.class.class)" )
  }
  else
    inherit = utils.class.class

  # create an environment in the parent_env which just contains the name of the inherited generator
  envir = new.env( parent = parent_env )
  utils.class.parent = inherit
  assign( "utils.class.parent", utils.class.parent, envir = envir )

  # add interfaces to R6 class
  if( !is.list( interfaces ) )
    interfaces = list( interfaces )

  interfaceNames = c()
  nInterfaces    = length( interfaces )
  if( nInterfaces )
  {
    publicNames  = names( public )
    privateNames = names( private )
    activeNames  = names( active )

    for( k in 1:nInterfaces )
    {
      if( interfaces[[ k ]]$inherit != "utils.class.interface.class" )
        stop( "interfaces must be inherited from utils.class.interface.class" )

      iName = interfaces[[ k ]]$classname

      # check public methods first
      for( iPublic in list( interfaces[[ k ]]$public_methods, interfaces[[ k ]]$public_fields ) )
        if( !is.null( iPublic ) )
          for( j in 1:length( iPublic ) )
          {
            iMethName =names( iPublic )[ k ]
            if( iMethName == "clone" )
              next();
            if( !( iMethName %in% names( public ) ) )
              stop( sprintf( "must implement public method %s on interface %s", iMethName, iName ) )
          }

      # check private methods
      for( iPrivate in list( interfaces[[ k ]]$private_methods, interfaces[[ k ]]$private_fields ) )
        if( !is.null( iPrivate ) )
          for( j in 1:length( iPrivate ) )
          {
            iMethName = names( iPrivate )[ k ]
            if( !( iMethName %in% names( private ) ) )
              stop( sprintf( "must implement private method %s on interface %s", iMethName, iName ) )
          }

      # check active methods
      iActive = interfaces[[ k ]]$active
      if( !is.null( iActive ) )
        for( j in 1:length( iActive ) )
        {
          iMethName = names( iActive )[ k ]
          if( !( iMethName %in% names( active ) ) )
            stop( sprintf( "must implement active field %s on interface %s", iMethName, iName ) )
        }

      interfaceNames[ length( interfaceNames ) + 1 ] = iName
    }
  }
  private$.INTERNAL_INTERFACES = c( inherit$private_fields$.INTERNAL_INTERFACES, interfaceNames )
  active$interfaces = function() return( private$.INTERNAL_INTERFACES )

  return( R6Class( classname = classname, public = public, private = private, active = active, inherit = utils.class.parent, lock_objects = lock_objects, class = class, portable = portable, lock_class = lock_class, cloneable = cloneable, parent_env = envir, lock ))
}

###################################################################################/
# utils.class.class ####
#
# add interfaces to R6 class infrastructure
###################################################################################/
utils.class.class = R6Class(
  "utils.class.class",
  private = list(
    .INTERNAL_INTERFACES = c()
  ),
  active = list(
    interfaces = function( val ) if( is.null( val ) ) return( private$.INTERNAL_INTERFACES ) else stop( "cannot update interface list manually" )
  )
)

###################################################################################/
# utils.class.interface.class ####
#
# add interfaces to R6 class infrastructure
###################################################################################/
utils.class.interface.class = R6Class(
  "utils.class.interface.class",
  public = list(
    is.interface  = function() return( TRUE )
  )
)

###################################################################################/
# utils.class.interface
# add interfaces to R6 class infrastructure
###################################################################################/
utils.class.interface = function(
  interfacename = NULL,
  public = list(),
  private = list(),
  active = list()
)
{
  return( R6Class( interfacename, public = public, private = private, active = active, inherit = utils.class.interface.class ) )
}

###################################################################################/
# utils.class.interface.implements
# checks to see if an interface has been implemented
# check private internal variable directly to prevent accidental name mismatches
###################################################################################/
utils.class.interface.implements = function(
  object,
  interfaceName
)
{
  if( !is.R6( object ) | !inherits( object, "utils.class.class") )
    stop( "object must be from a class generated by utils.class()" )

  if( is.null( object$.__enclos_env__$private$.INTERNAL_INTERFACES ) )
    stop( "object must be from a class generated by utils.class()" )

  return( length( intersect( object$.__enclos_env__$private$.INTERNAL_INTERFACES, interfaceName ) ) == 1 )
}

###################################################################
# Name:       utils.list.namedListToChar
# Descrption: takes a named list of characters/doubles and converts it to a singe character
# Args:       list
# Return:     character
###################################################################
utils.list.namedListToChar = function( list, delimiter = "¶" )
{
  if( !is.list( list ) )
    stop( "list must be a list" )

  names  = names( list )
  values = as.character( list );
  if( is.null(names) )
    stop( "list must be a named list" )

  char = paste( c( names, values ), collapse = delimiter );

  if( stringr::str_count( char, pattern = delimiter ) != 2 * length( names ) - 1 )
    stop( sprintf( "list cannot contain the delimiter character %s", regex = delimiter ) )

  return( char )
}

###################################################################
# Name:       utils.list.charToNamedList
# Descrption: inverse of utils.list.charToNamedList
# Args:       char
# Return:     namedlist
#             if type is set to NULL then we convert to a numeric
#             value of possible, failing that will convert to a character
###################################################################
utils.list.charToNamedList = function( char, delimiter = "¶", type = NULL )
{
  if( length( char ) > 1 )
    stop( "character must be of a single length" )

  bits = strsplit( char, delimiter )[[1]];
  n    = length( bits ) / 2;

  if( round( n ) != n )
    stop( sprintf( "char must contain the delimiter %s an odd number of times", delimiter ) )

  if( is.null( type ) )
  {
    listChar = as.list( bits[ ( n + 1):( 2 * n ) ] )
    list     = as.list( suppressWarnings( as.double( bits[ ( n + 1):( 2 * n ) ] ) ) )
    list     = ifelse( is.na( list ), listChar, list )
  }
  else if( type == "character" )
    list = as.list( bits[ ( n + 1):( 2 * n ) ] )
  else
    if( type == "double" )
      list = as.list( suppressWarnings( as.double( bits[ ( n + 1):( 2 * n ) ] ) ) )
  else

    stop( "only suports type of character and double" );

  return( setNames( list, bits[ 1:n ]) )
}

###################################################################/
# Name:       utils.data.table.containsColumns
# Descrption: takes data.table checks to see if it contains columns
# Args:       dataTable    - a data.table
#             columns      - a vector of column names
# Return:     TRUE/FALSE
###################################################################/
utils.data.table.containsColumns = function( dataTable, columns )
{
  if( !is.data.table( dataTable ) )
    stop( "dataTable must be a data.table")

  if( length( intersect( names( dataTable), columns ) ) != length( columns ) )
    return( FALSE )
  else
    return( TRUE );
}

##################################################################/
# Name:        utils.shiny.getLastEvent.clear
# Description: function for working out the last event of of a subest
###################################################################/
utils.shiny.getLastEvent.clear = function()
{
  if( exists( "UTILS_SHINY_APP_LASTEVENT", envir = cacheEnv ) )
    assign( "UTILS_SHINY_APP_LASTEVENT", NULL, envir = cacheEnv )
}

##################################################################/
# Name:        utils.shiny.getLastEvent
# Description: function for working out the last event of of a subest
###################################################################/
utils.shiny.getLastEvent = function( eventList, name )
{
  maxRows = 1e3;
  if( !exists( "UTILS_SHINY_APP_LASTEVENT", envir = cacheEnv ) )
  {
    lastEvent = data.table( NAME = rep( "", maxRows ), INDEX = rep( as.integer( 0 ), maxRows ), DETAILS = rep( "", maxRows ) )
    assign( "UTILS_SHINY_APP_LASTEVENT", lastEvent, envir = cacheEnv )
  }
  else
    lastEvent = get( "UTILS_SHINY_APP_LASTEVENT", envir = cacheEnv )

  nEvents = length( eventList );

  # add a new row to lastEvent if necessary
  if( lastEvent[ NAME == name ][ ,.N ] == 0 )
  {
    nextIdx = lastEvent[ ,.N ] - lastEvent[ NAME == "" ][ ,.N ] + 1;
    names   = rep( name, nEvents + 1 );

    if( nextIdx + nEvents > lastEvent[ ,.N ])
    {
      blankEvent = data.table( NAME = rep( "", maxRows ), INDEX = rep( 0, maxRows ), DETAILS = rep( "", maxRows ) )
      lastEvent  = rbindlist( list( lastEvent, blankEvent ), use.names = TRUE )
    }

    lastEvent[ nextIdx:( nextIdx + nEvents ), c( "NAME", "INDEX" ) := list( names, 0:nEvents ) ];
  }

  # turn event into a string for storing in the lastEvent table
  details = c();
  for( k in 1:nEvents)
    if( is.list( eventList[[ k ]]) )
      details[ k ] = utils.list.namedListToChar( eventList[[ k ]] )
  else
    details[ k ] = paste( as.character( eventList[[ k ]]), collapse = ":" )

  # ignore certain events
  ignoreEvents = c( "dragmode¶zoom", "dragmode¶pan", "dragmode¶select", "dragmode¶lasso" );
  for( k in 1:nEvents )
  {
    if( sum( ignoreEvents == details[ k ] ) > 0 )
      eventList[[ k ]] = NA  # use NA since if set last element of list to NULL it is dropped :(
  }

  # ignore events which have not happened yet
  nonNullIdxs = c();
  for( k in 1:nEvents )
    if( !is.null( eventList[[ k ]] ) &&  !is.na( eventList[[ k ]] ) )
      nonNullIdxs = c( nonNullIdxs, k )

  # most recent event deemed to be first one which has changed
  if( length( nonNullIdxs) >  0)
    for( k in 1:length( nonNullIdxs ) )
    {
      idx = nonNullIdxs[ k ];
      if( lastEvent[ NAME == name & INDEX == idx, DETAILS ] != details[ idx ])
      {
        lastEvent[ NAME == name & INDEX == idx, DETAILS := details[ idx ] ];
        lastEvent[ NAME == name & INDEX == 0,   DETAILS := as.character( idx ) ];
        assign( "UTILS_SHINY_APP_LASTEVENT", lastEvent, envir = cacheEnv )
        return( list( index = idx, event = eventList[[ idx ]] ) );
      }
    }

  # if no event has changed then the most recent event
  lastIdx = as.integer( lastEvent[ NAME == name & INDEX == 0, DETAILS ] );
  if( !is.null( lastIdx ) && !is.na( as.double( lastIdx ) ) )
  {
    details = lastEvent[ NAME == name & INDEX == lastIdx, DETAILS ];
    if( !is.null( details ) & is.character( details ) )
      return( list( index = lastIdx, event = utils.list.charToNamedList( details ) ) )
  }

  return( NULL );
};

###################################################################################/
# utils.shiny.element ####
###################################################################################/
utils.shiny.element = utils.class(
  classname = "utils.shiny.element",
  private = list(
    .id             = NULL,
    .ui             = NULL,
    .server         = NULL,
    .callBacks      = list()
  ),
  public = list(
    .reactiveValues = reactiveValues(),

    ###################################################################################/
    # initialize
    ###################################################################################/
    initialize = function( ui = NULL, server = NULL , callBacks = list(), reactiveValues = list() )
    {
      private$.id          <- UUIDgenerate()
      private$.ui          <- ui
      private$.server      <- server
      private$.callBacks   <- callBacks
      self$.reactiveValues <- reactiveValues
    },

    ###################################################################################/
    # runApp
    ###################################################################################/
    runApp = function() return( runApp( shinyApp( ui = self$ui, server = self$server ) ) ),

    ###################################################################################/
    # show
    ###################################################################################/
    show = function() return( self$runApp() ),

    ###################################################################################/
    # setRV
    ###################################################################################/
    setRV = function( name, value )
    {
      self$reactiveValues$setRV( name, value )
    },

    ###################################################################################/
    # getRV
    ###################################################################################/
    getRV = function( name )
    {
      return( self$reactiveValues$getRV( name ) )
    }

  ),

  active = list(
    ###################################################################################/
    # id
    ###################################################################################/
    id = function() return( private$.id ),

    ###################################################################################/
    # ui
    ###################################################################################/
    ui = function() return( private$.ui ),

    ###################################################################################/
    # server
    ###################################################################################/
    server = function() return( private$.server ),

    ###################################################################################/
    # callBacks
    ###################################################################################/
    callBacks = function() return( private$.callBacks ),

    ###################################################################################/
    # reactiveValues
    ###################################################################################/
    reactiveValues = function() return( self$.reactiveValues )
  )
);

###################################################################/
# utils.plotly.scatter.class ####
###################################################################/
utils.plotly.scatter.class = utils.class(
  classname = "utils.plotly.scatter.class",
  inherit = utils.shiny.element,
  public = list(
    ###################################################################################/
    # initialize
    ###################################################################################/
    initialize = function( ui = NULL, server = NULL , callBacks = list(), reactiveValues = list() )
    {
      super$initialize( ui = ui, server = server, callBacks = callBacks, reactiveValues = reactiveValues )
    },
    ###################################################################################/
    # update
    ###################################################################################/
    update = function()
    {
      self$setRV( "update", self$getRV( "update" ) + 1 )
    }
  ),
  active = list(
    ###################################################################################/
    #  data
    ###################################################################################/
    data = function( data )
    {
      if( missing( data ) )
        return( self$getRV( "data") )
      else
      {
        self$setRV( "data", data )
        self$update()
      }
    }
  )
)


###################################################################/
# utils.plotly.scatter ####
###################################################################/
utils.plotly.scatter = function(
  # which data to display
  data,
  columns,
  colorCol = NULL,
  textCol  = NULL,
  sizeCol  = NULL,
  shapes   = list(),

  # beautification
  xAxisTitle    = "x",
  yAxisTitle    = "y",
  colorBarTitle = NULL,
  scatterType   = "scatter",
  mode          = "markers",
  xLabelFunc    = NULL,
  width         = 700,
  height        = 450,

  # extensions to allow more complicated graphs
  chartClass = utils.plotly.scatter.class,
  extraRVs   = list(),

  # data control
  copyData = TRUE
)
{
  # unique ID for this this particular plot
  chartId = UUIDgenerate()
  dummyId = sprintf( "%s_dummy", chartId )

  # if more than 2 columns specified then add column selectors to graph
  if( length( columns ) < 2 )
    stop( "must specify at least 2 columns")
  else
    if( length( columns ) == 2 )
    {
      # no frills layout
      ui = fluidPage(
        plotlyOutput( chartId ),
        htmlOutput( dummyId )
      )
    }
  else
  {
    # layout with column selectors
    xInputId = sprintf( "%s_xInput", dummyId )
    xInput   = selectInput(
      inputId = xInputId,
      label   = "x-axis",
      choices = columns,
      width = "100%",
      selected = columns[ 1 ]
    )
    yInputId = sprintf( "%s_yInput", dummyId )
    yInput   = selectInput(
      inputId = yInputId,
      label   = "y-axis",
      choices = columns,
      width = "100%",
      selected = columns[ 2 ]
    )

    ui =fluidPage(
      div( style = "height: 60px", fillRow(  column( 12, xInput ), column( 12, yInput ) ) ),
      fluidPage( column( width = 12, plotlyOutput( chartId ) ) ),
      fluidRow( column( width = 12, htmlOutput( dummyId ) ) )
    )
  }

  # set up ability to add callbacks
  setUpEnv        = environment();
  onPointSelected = list();
  addOnPointSelectedCallBack = function( callBack )
  {
    onPointSelected = get( "onPointSelected", envir =  setUpEnv )
    onPointSelected[[ length( onPointSelected ) + 1 ]] = callBack;
    assign( "onPointSelected", onPointSelected, envir = setUpEnv )
  }

  onRelayout = list();
  addOnRelayoutCallBack = function( callBack )
  {
    onRelayout = get( "onRelayout", envir =  setUpEnv )
    onRelayout[[ length( onRelayout ) + 1 ]] = callBack;
    assign( "onRelayout", onRelayout, envir = setUpEnv )
  }

  # reactive values
  getRV <- function( name )
  {
    RV =  get( "RV", envir =  setUpEnv )
    return( RV[[ name ]] )
  }
  setRV <- function( name, value )
  {
    RV = get( "RV", envir =  setUpEnv )
    RV[[ name ]] = value;
  }

  # update old is a non-reactivevalue which we use to keep track of updates
  assign( "updateOld", 0, envir = setUpEnv );
  assign( "eventOld", list( x = 0 ), envir = setUpEnv );

  # copy of data for chart and add index
  if( copyData )
    DATA = copy( data )
  else
    DATA = data

  # is the data shown in the plot
  if( !utils.data.table.containsColumns( DATA, "INTERNAL_SHINY_SHOW" ) )
    DATA[ , INTERNAL_SHINY_SHOW := TRUE ]

  # get the data range
  xcol = columns[ 1 ]
  XMIN = DATA[ , min( get( xcol ) ) ]
  XMAX = DATA[ , max( get( xcol ) ) ]

  # the basic plot
  getPlot = function( DATA, xCol, yCol, xmin, xmax, ymin, ymax, shapes )
  {
    # add index to data to uniquely label points
    DATA[ , INTERNAL_SHINY_INDEX := 1:DATA[ ,.N ] ]

    # is the data shown in the plot
    if( !utils.data.table.containsColumns( DATA, "INTERNAL_SHINY_SHOW" ) )
      DATA[ , INTERNAL_SHINY_SHOW := TRUE ]

    # set up colour columns
    if( is.null( colorCol) )
      DATA[ , INTERNAL_SHINY_COLOR := "" ]
    else
      DATA[ , INTERNAL_SHINY_COLOR := get( colorCol ) ]

    # set up colour columns
    if( is.null( textCol) )
      DATA[ , INTERNAL_SHINY_TEXT := "" ]
    else
      DATA[ , INTERNAL_SHINY_TEXT := get( textCol ) ]

    # set up size columns
    if( is.null( sizeCol ) )
      DATA[ , INTERNAL_SHINY_SIZE := 1 ]
    else
      DATA[ , INTERNAL_SHINY_SIZE := get( sizeCol ) ]

    # store copy of decorated data
    assign( "data", DATA, envir = setUpEnv );

    # set up marker details
    marker = list( sizemode = 'area' )
    if( !is.null( colorBarTitle ) )
      marker[[ "colorbar" ]]  = list( title = colorBarTitle )

    # details of the axis
    xaxis = list( title = xAxisTitle, range = list( xmin, xmax ) )
    yaxis = list( title = yAxisTitle, range = list( ymin, ymax ) )
    if( !is.null( xLabelFunc ) )
    {
      xaxis$tickmode = "array"
      if( is.null( xmin ) )
        xaxis = c( xaxis, xLabelFunc( XMIN, XMAX ) )
      else
        xaxis = c( xaxis, xLabelFunc( xmin, xmax ) )
    }

    # generate the plot
    plot = plot_ly(
      DATA[ INTERNAL_SHINY_SHOW == TRUE ],
      x     = ~get( xCol ),
      y     = ~get( yCol ),
      color = ~INTERNAL_SHINY_COLOR,
      key   = ~INTERNAL_SHINY_INDEX,
      text  = ~INTERNAL_SHINY_TEXT,
      size  = ~INTERNAL_SHINY_SIZE,
      source = chartId,
      fill = ~'',
      marker = marker,
      type = scatterType,
      mode = mode,
      width = width,
      height = height
    ) %>%
      layout(
        plot,
        xaxis = xaxis,
        yaxis = yaxis,
        shapes = shapes
      )
    return( plot )
  }

  server = function( input, output )
  {
    assign( "RV", reactiveValues( data = DATA, update = 0, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, shapes = shapes ), envir = setUpEnv );

    output[[ chartId ]] <- renderPlotly(
      {
        # dummy call to update to force update
        update = getRV( "update" )

        # get columns
        xaxis = ifelse( length( columns ) > 2, input[[ xInputId ]], columns[ 1 ] )
        yaxis = ifelse( length( columns ) > 2, input[[ yInputId ]], columns[ 2 ] )
        getPlot( getRV( "data" ), xaxis, yaxis, getRV( "xmin" ), getRV( "xmax" ), getRV( "ymin" ), getRV( "ymax" ), getRV( "shapes")  )
      } )

    output[[ dummyId ]] <- renderText(
      {
        # trigger callbacks on update
        update    =  getRV( "update" )
        updateOld =  get( "updateOld", envir =  setUpEnv )

        events      = list( event_data( "plotly_click", source = chartId ), event_data( "plotly_relayout", source = chartId ) )
        latestEvent = utils.shiny.getLastEvent( events, chartId )
        eventOld    =  get( "eventOld", envir =  setUpEnv )

        if( update != updateOld | is.null( latestEvent ) )
          assign( "updateOld", update, envir = setUpEnv )
        else
          if( utils.list.namedListToChar( latestEvent ) != utils.list.namedListToChar( eventOld ) )
          {
            assign( "eventOld", latestEvent, envir = setUpEnv )
            if( latestEvent$index == 1 )
            {
              latestClick = latestEvent$event
              # run any callbacks if necessary
              if( !is.null( latestClick ) )
              {
                pointSelect = latestClick[ 1, "key" ]
                DATA        = get( "data", envir = setUpEnv )
                rowData     = DATA[ INTERNAL_SHINY_INDEX == pointSelect ]

                onPointSelected = get( "onPointSelected", envir =  setUpEnv )
                if( length( onPointSelected ) )
                  for( i in 1:length( onPointSelected) )
                    onPointSelected[[ i ]]( rowData )
              }
            }
            else
              if( latestEvent$index == 2 )
              {
                # deal with layout changes, since we are constantly redrawing then need to remember the last size
                latestLayout = latestEvent$event

                xmin = NA
                xmax = NA
                ymin = NA
                ymax = NA
                if( !is.null( latestLayout$'xaxis.range[0]'))
                {
                  xmin = latestLayout$'xaxis.range[0]'
                  setRV( "xmin", xmin )
                }
                if( !is.null( latestLayout$'xaxis.range[1]'))
                {
                  xmax = latestLayout$'xaxis.range[1]'
                  setRV( "xmax", xmax )
                }
                if( !is.null( latestLayout$'yaxis.range[0]'))
                {
                  ymin = latestLayout$'yaxis.range[0]'
                  setRV( "ymin", ymin )
                }
                if( !is.null( latestLayout$'yaxis.range[1]'))
                {
                  ymax = latestLayout$'yaxis.range[1]'
                  setRV( "ymax", ymax )
                }
                if( !is.null( latestLayout$'xaxis.autorange' ) )
                {
                  xmin = xmax = NULL
                  setRV( "xmin", NULL )
                  setRV( "xmax", NULL )
                }
                if( !is.null( latestLayout$'yaxis.autorange' ) )
                {
                  ymin = ymax = NULL
                  setRV( "ymin", NULL )
                  setRV( "ymax", NULL )
                }

                onRelayout = get( "onRelayout", envir =  setUpEnv )
                if( length( onRelayout ) )
                  for( i in 1:length( onRelayout ) )
                    onRelayout[[ i ]]( xmin, xmax, ymin, ymax )
              }
          }
        ""
      } )
  }

  return( chartClass$new( server = server, ui = ui, callBacks = list( addOnPointSelectedCallBack = addOnPointSelectedCallBack, addOnRelayoutCallBack = addOnRelayoutCallBack ), list( getRV = getRV, setRV = setRV ) ) )
}
