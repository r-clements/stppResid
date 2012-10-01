\name{print.stwin}
\alias{print.stwin}
\title{
Print details of a \code{stwin} object
}
\description{
Prints the details of a space-time window.
}
\usage{
\method{print}{stwin}(x, \ldots)
}
\arguments{
  \item{x}{
A \dQuote{\code{stwin}} object.
}
  \item{\ldots}{
  }
}
\author{
Robert Clements
}
\seealso{
\code{\link{stwin}}
}
\examples{
stw <- stwin(c(0,2), c(0,5), c(0,10))
stw
}