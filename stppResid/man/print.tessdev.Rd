\name{print.tessdev}
\alias{print.tessdev}
\title{
Print details of a \code{tessdev} object
}
\description{
Prints the details of the results of finding tessellation deviance residuals for a space-time point process.
}
\usage{
\method{print}{tessdev}(x, \ldots)
}
\arguments{
  \item{x}{
A \dQuote{\code{tessdev}} object.
}
  \item{\ldots}{
  }
}
\details{
Prints the details of the space-time dataset, the tessellation residuals, and the tile list.

The tile list is a list of the details of the tessellation cells.
}
\author{
Robert Clements
}
\seealso{
\code{\link{tessdev}}
}