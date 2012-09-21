\name{plot.thinresid}
\alias{plot.thinresid}
\title{
Plot thinned residuals
}
\description{
Plot thinned residuals for a space-time point process.
}
\usage{
plot.thinresid(X, pch = 1, asp = 1, ...)
}
\arguments{
  \item{X}{
A \code{"thinresid"} object.
}
  \item{pch}{
Character type for plotting the points.
}
  \item{asp}{
y/x aspect ratio.
}
  \item{\dots}{
Arguments for use with \code{\link{plot}}.
}
}
\details{
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
