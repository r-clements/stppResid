\name{summary.supresid}
\alias{summary.supresid}
\title{
Summary of a set of superposed residuals
}
\description{
Outputs and prints a summary of a set of superposed residuals.
}
\usage{
\method{summary}{supresid}(object, \ldots)
}
\arguments{
  \item{object}{
A \dQuote{\code{supresid}} object.
}
  \item{\ldots}{
  }
}
\details{
Outputs and prints a summary of \code{supresid} object.

Printed to the screen are the superposition rate, \code{k}, the number of superposed residuals, \code{n}, the expected number of residuals, \code{n.exp}, and the p-value for observing \code{n} residuals (\code{p.val}).
}
\value{
A list of 
 	\item{k }{ The superposition rate.}
 	\item{n }{ The number of residuals.}
 	\item{n.exp }{ The expected number of residuals.}
 	\item{p.val }{ The p-value for \code{n}.}
}
\author{
Robert Clements
}
\seealso{
\code{\link{supresid}}
}
\examples{
data(sresiduals1)
summary(sresiduals1)

data(sresiduals2)
summary(sresiduals2)
}
