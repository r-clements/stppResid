\name{print.thinresid}
\alias{print.thinresid}
\title{
Print details of a \code{thinresid} object
}
\description{
Prints the details of the results of finding thinned residuals for a space-time point process.
}
\usage{
\method{print}{thinresid}(x, \ldots)
}
\arguments{
  \item{x}{
A \dQuote{\code{thinresid}} object.
}
  \item{\ldots}{
  }
}
\details{
Prints the details of the space-time dataset, the thinning rate, the thinned residuals, and the removed points.
}
\author{
Robert Clements
}
\seealso{
\code{\link{thinresid}}
}
\examples{
data(tresiduals1)
tresiduals1
}