\name{print.gridresid}
\alias{print.gridresid}
\title{
Print details of a \code{gridresid} object
}
\description{
Prints the details of the results of finding grid-based residuals for a space-time point process.
}
\usage{
\method{print}{gridresid}(x, \ldots)
}
\arguments{
  \item{x}{
A \dQuote{\code{gridresid}} object.
}
  \item{\ldots}{
  }
}
\details{
Prints the details of the space-time dataset, the grid-based residuals, and the spatial grid.
}
\author{
Robert Clements
}
\seealso{
\code{\link{gridresid}}
}
\examples{
data(gresiduals)
gresiduals
}
