\name{print.tessresid}
\alias{print.tessresid}
\title{
Print details of a \code{tessresid} object
}
\description{
Prints the details of the results of finding tessellation residuals for a space-time point process.
}
\usage{
\method{print}{tessresid}(x, \ldots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A \dQuote{\code{tessresid}} object.
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
\code{\link{tessresid}}
}
\examples{
data(tsresiduals)
tsresiduals
}
