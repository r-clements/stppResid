\name{print.supresid}
\alias{print.supresid}
\title{
Print details of a \code{supresid} object
}
\description{
Prints the details of the results of finding superposed residuals for a space-time point process.
}
\usage{
\method{print}{supresid}(x, \ldots)
}
\arguments{
  \item{x}{
A \dQuote{\code{supresid}} object.
}
  \item{\ldots}{
  }
}
\details{
Prints the details of the space-time dataset, the superposition rate, the superposed residuals, and the simulated points.
}
\author{
Robert Clements
}
\seealso{
\code{\link{supresid}}
}
\examples{
data(sresiduals1)
sresiduals1
}
