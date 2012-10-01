\name{print.devresid}
\alias{print.devresid}
\title{
Print details of a \code{devresid} object
}
\description{
Prints the details of the results of finding deviance residuals for a space-time point process.
}
\usage{
\method{print}{devresid}(x, \ldots)
}
\arguments{
  \item{x}{
A \dQuote{\code{devresid}} object.
}
  \item{\ldots}{
  }
}
\details{
Prints the details of the space-time dataset, the deviance residuals, and the spatial grid.
}
\author{
Robert Clements
}
\seealso{
\code{\link{devresid}}
}
\examples{
data(deviance)
deviance
}
