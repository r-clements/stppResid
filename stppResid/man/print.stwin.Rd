\name{print.stwin}
\alias{print.stwin}
\title{
Print details of a \code{stwin} object
}
\description{
Prints the details of a space-time window.
}
\usage{
print.stwin(x)
}
\arguments{
  \item{x}{
A \code{\dQuote{stwin}} object.
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