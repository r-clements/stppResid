\name{print.stgrid}
\alias{print.stgrid}
\title{
Print details of a \code{stgrid} object
}
\description{
Prints a full spatial grid.
}
\usage{
\method{print}{stgrid}(x, \ldots)
}
\arguments{
  \item{x}{
A \dQuote{\code{stgrid}} object.
}
  \item{\ldots}{
  }
}
\author{
Robert Clements
}
\seealso{
\code{\link{make.grid}}
}
\examples{
data(gresiduals)
gresiduals$grid
}
