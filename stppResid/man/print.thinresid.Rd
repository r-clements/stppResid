\name{print.thinresid}
\alias{print.thinresid}
\title{
Print details of a \code{thinresid} object
}
\description{
Prints the details of the results of finding thinned residuals for a space-time point process.
}
\usage{
print.thinresid(x)
}
\arguments{
  \item{x}{
A \code{\dQuote{thinresid}} object.
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