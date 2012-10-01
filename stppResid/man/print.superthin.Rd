\name{print.superthin}
\alias{print.superthin}
\title{
Print details of a \code{superthin} object
}
\description{
Prints the details of the results of finding super-thinned residuals for a space-time point process.
}
\usage{
\method{print}{superthin}(x, \ldots)
}
\arguments{
  \item{x}{
A \dQuote{\code{superthin}} object.
}
  \item{\ldots}{
  }
}
\details{
Prints the details of the space-time dataset, the super-thinning rate, the super-thinned residuals, the simulated points, the automatically retained points, the retained points after thinning, and the removed points.
}
\author{
Robert Clements
}
\seealso{
\code{\link{superthin}}
}
\examples{
data(stresiduals1)
stresiduals1
}
