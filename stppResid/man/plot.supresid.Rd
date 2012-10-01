\name{plot.supresid}
\alias{plot.supresid}
\title{
Plot superposed residuals
}
\description{
Plot superposed residuals for a space-time point process.
}
\usage{
\method{plot}{supresid}(x, \ldots, pch1 = 1, pch2 = 3, asp = 1)
}
\arguments{
  \item{x}{
A \dQuote{\code{supresid}} object.
}
    \item{\ldots}{
Arguments for use with \code{\link{plot}}.
}
  \item{pch1}{
Character type for plotting the original data points.
}
  \item{pch2}{
Character type for plotting the simulated points.
}
  \item{asp}{
y/x aspect ratio.
}
}
\details{
Plots the superposed residuals, where the original points of the point pattern can be plotted as a separate symbol than the superposed (simulated) points. 
}
\author{
Robert Clements
}
\seealso{
\code{\link{supresid}}
}
\examples{
data(sresiduals1)
plot(sresiduals1, pch1 = 2)

data(sresiduals2)
plot(sresiduals2)
}