\name{plot.stpp}
\alias{plot.stpp}
\title{
Plot a space-time point pattern
}
\description{
Plot a space-time point pattern described by a \code{\dQuote{stpp}} object.
}
\usage{
plot.stpp(X, pch = 1, asp = 1, ...)
}
\arguments{
  \item{X}{
A \code{\dQuote{stpp}} object.
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
\code{\link{stpp}}, \code{\link{stwin}}
}
\examples{
data(eq)
plot(eq)
}
