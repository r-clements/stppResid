\name{plot.superthin}
\alias{plot.superthin}
\title{
Plot super-thinned residuals
}
\description{
Plot super-thinned residuals for a space-time point process.
}
\usage{
\method{plot}{superthin}(x, \ldots, pch1 = 1, pch2 = 3, asp = 1)
}
\arguments{
  \item{x}{
A \dQuote{\code{superthin}} object.
}
    \item{\ldots}{
Arguments for use with \code{\link{plot}}.
}
  \item{pch1}{
Character type for plotting the retained points from the original data set.
}
  \item{pch2}{
Character type for plotting the simulated points.
}
  \item{asp}{
y/x aspect ratio.
}
}
\details{
Plots the super-thinned residuals, where the original points of the point pattern can be plotted as a separate symbol than the superposed (simulated) points.
}
\author{
Robert Clements
}
\note{
This function does not plot the time component from the super-thinned residuals, only the spatial coordinates.
}
\seealso{
\code{\link{superthin}}
}
\examples{
data(stresiduals1)
plot(stresiduals1, pch1 = 3, pch2 = 5)
}