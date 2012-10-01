\name{plot.stpp}
\alias{plot.stpp}
\title{
Plot a space-time point pattern
}
\description{
Plot a space-time point pattern described by a \dQuote{\code{stpp}} object.
}
\usage{
\method{plot}{stpp}(x, \ldots, pch = 1, asp = 1)
}
\arguments{
  \item{x}{
A \dQuote{\code{stpp}} object.
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
\code{\link{stpp}}, \code{\link{stwin}}
}
\examples{
data(eq)
plot(eq)
}
