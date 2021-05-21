\name{treeplot.tree}
\alias{treeplot.tree}
\title{
Interactive Tree Plotting Package
}
\description{
This function plots trees with timed events (e.g. transmissions or phylogentic trees).
Since trees can be very large, the plots are interactive and to allow zooming into places of interest.
For large trees not all edges are plotted to speed up the time to render (but are dynamically added whne zooming).
}
\usage{
treeplot.tree( parent_ids, child_ids, times)
}
\arguments{
  \item{parent_ids}{a vector of IDs of the parent nodes}
  \item{child_ids}{a vector of IDs of the child nodes (for root nodes child_id = parent_id)}
  \item{time}{a vector of times at which the transmissions took place}
  \item{colors}{a vector of IDs used to color and label each node}
  \item{max_edges}{the maxmimum number of edges to plot (default = \code{1000})}
  \item{interactive}{allow full interactive plotting (default = \code{TRUE})  }
  \item{show}{show the Shiny/Plotly object or return it (default = \code{TRUE})  }
  \item{height}{the height of the plot (default = \code{1000})  }
}

\examples{
> library( data.table )
> library( RTreePlot )
>
> dt = fread( system.file("data/smallTree.csv", package = "RTreePlot") )
> colors = sample( c("a","b","c"), dt[,.N], replace = TRUE )
> treeplot.plot( dt[ , parent_ids], dt[ , child_ids], dt[,times], colors, interactive = FALSE)
>
> dt = fread( system.file("data/largeTree.csv", package = "RTreePlot") )
> treeplot.plot( dt[ , parent_ids], dt[ , child_ids], dt[,times], interactive = TRUE )
>
}
