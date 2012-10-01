\name{plot.gridresid}
\alias{plot.gridresid}
\title{
Plot grid-based residuals
}
\description{
\code{plot.gridresid} is a generic function for plotting grid-based residuals.
}
\usage{
\method{plot}{gridresid}(x, \ldots, col.key = rev(heat.colors(100)), cutoffs = NULL, 
plot.smooth = FALSE, smooth.col = heat.colors, smooth.key = FALSE, contours = 
FALSE, nlevels = 20)
}
\arguments{
  \item{x}{
A \dQuote{\code{gridresid}} object.
}
    \item{\ldots}{
Arguments for use with \code{\link{points}}.
}
  \item{col.key}{
A vector of colors in hexadecimal format.
}
  \item{cutoffs}{
A vector of cut points for assigning the colors in \code{col.key} to the residuals in \code{X}. \code{cutoffs} should be a vector of length one more than the length of \code{col.key}.
}
  \item{plot.smooth}{
Plot a smoothed version of the residuals.
}
  \item{smooth.col}{
A vector of colors in hexadecimal format.
}
  \item{smooth.key}{
If \code{TRUE}, a color key will be plotted.  
}
  \item{contours}{
If \code{TRUE}, contour lines will be plotted.  
}
  \item{nlevels}{
Number of partitions of the residuals to be plotted.
}
}
\details{
\code{cutoffs} must be a vector of increasing values of the same length as \code{col.key} plus 1. \code{cutoffs} divides the residual values in \code{x$residuals} into a number of intervals equal to the number of colors in \code{col.key}. The colors are assigned to the intervals in order, e.g. the first color in \code{col.key} will be plotted in the bins defined by the spatial grid in \code{x$grid} that contains a residual that falls anywhere in the first interval (lower bound inclusive, upper bound exclusive).  
	
Default \code{col.key} is a vector of 100 \code{heat} colors in reverse. Default \code{cutoffs} is a vector of 101 equally spaced points that range from the minimum residual in \code{x$residuals}, minus a very small number, to the maximum residual, plus a very small number.
}
\note{
The default \code{col.key} and \code{cutoffs} may not be useful if the residuals are highly skewed. In this case, there should be more values in \code{cutoffs} where the residuals are most dense.

These are simply default plots for quick illustration of the residuals, and may or may not be useful for detailed analysis of the residuals.
}
\author{
Robert Clements
}
\seealso{
\code{\link{gridresid}}, \code{\link{image}}
}
\examples{
data(gresiduals)
plot(gresiduals)

hist(gresiduals$residuals)
cutoffs = c(seq(-2.74, -1.01, length.out = 15), 
seq(-1, 1, length.out = 70), seq(1, 3.83, length.out = 16))
plot(gresiduals, cutoffs = cutoffs)

plot(gresiduals, plot.smooth = TRUE, contours = TRUE)
}

