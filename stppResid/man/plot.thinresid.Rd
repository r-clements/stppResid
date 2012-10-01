\name{plot.thinresid}
\alias{plot.thinresid}
\title{
Plot thinned residuals
}
\description{
Plot thinned residuals for a space-time point process.
}
\usage{
\method{plot}{thinresid}(x, \ldots, pch = 1, asp = 1)
}
\arguments{
  \item{x}{
A \dQuote{\code{thinresid}} object.
}
  \item{\ldots}{
Arguments for use with \code{\link{plot}}.
}
  \item{pch}{
Character type for plotting the points.
}
  \item{asp}{
y/x aspect ratio.
}
}
\author{
Robert Clements
}
\seealso{
\code{\link{thinresid}}
}
\examples{
data(tresiduals1)
plot(tresiduals1)

data(tresiduals2)
plot(tresiduals2)
}
