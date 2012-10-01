\name{make.grid}
\alias{make.grid}
\title{
Make a grid of spatial bins
}
\description{
\code{make.grid} creates a space-time grid object of class {\dQuote{stgrid}} for use with the functions \code{\link{gridresid}} and \code{\link{devresid}}. 
}
\usage{
make.grid(stw, grid = c(10, 10))
}
\arguments{
  \item{stw}{
A space-time window object of class \dQuote{\code{stwin}} to be divided into space-time bins.
}
  \item{grid}{
A vector representing the number of rows and columns in the grid.
}
}
\details{
The space-time window, \code{stw}, is divided into a grid of space-time bins using the \code{grid} argument to determine the number of intervals in the x and y directions. The total number of bins will be the product of the elements of \code{grid}. 

If \code{grid} is not specified, the default grid is 10 x 10, resulting in 100 total bins.
}
\value{
Outputs an object of class \dQuote{\code{stgrid}}, which is a list of
	\item{grid.full }{ A data frame where each row represents a bin. There are four columns, \code{xmin}, \code{xmax}, \code{ymin}, and \code{ymax}, which represent the x and y limits of the bins.}
}
\author{
Robert Clements
}
\seealso{
\code{\link{stwin}}, \code{\link{gridresid}}
}
\examples{
#===> create a space-time window  <===#
xcoord <- c(0, 10)
ycoord <- c(0, 20)
tcoord <- c(0, 100)
stw <- stwin(xcoord, ycoord, tcoord)

#===> create a 10 x 10 grid <===#
gf <- make.grid(stw)

#===> create a 10 x 20 grid <===#
gf <- make.grid(stw, c(10, 20))
nrow(gf$grid.full)
ncol(gf$grid.full)
}