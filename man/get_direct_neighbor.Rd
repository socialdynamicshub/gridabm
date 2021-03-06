% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_direct_neighbor.R
\name{get_direct_neighbor}
\alias{get_direct_neighbor}
\title{Get neighbor position at given orientation}
\usage{
get_direct_neighbor(i, j, dims, orientation)
}
\arguments{
\item{i}{Row index.}

\item{j}{Column index.}

\item{dims}{Vector containing board dimensions (rows, columns).}

\item{orientation}{One of "north", "east", "south", and "west".}
}
\value{
The position of the neighbor.
}
\description{
Get neighbor position at given orientation
}
\examples{
get_direct_neighbor(10, 10, c(20, 20), "north")
}
