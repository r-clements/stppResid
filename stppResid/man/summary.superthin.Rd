\name{summary.superthin}
\alias{summary.superthin}
\title{
Summary of a set of super-thinned residuals 
}
\description{
Outputs and prints a summary of a set of super-thinned residuals.
}
\usage{
\method{summary}{superthin}(object, \ldots)
}
\arguments{
  \item{object}{
A \dQuote{\code{superthin}} object.
}
  \item{\ldots}{
  }
}
\details{
Outputs and prints a summary of the \code{superthin} object.

Printed to the screen are the super-thinning rate, \code{k}, the number of super-thinned residuals, \code{n}, the expected number of residuals, \code{n.exp}, and the p-value for observing \code{n} residuals (\code{p.val}).
}
\value{
A list of 
 	\item{k }{ The super-thinning rate.}
 	\item{n }{ The number of residuals.}
 	\item{n.exp }{ The expected number of residuals.}
 	\item{p.val }{ The p-value for \code{n}.}
}
\author{
Robert Clements
}
\seealso{
\code{\link{superthin}}
}
\examples{
data(stresiduals1)
summary(stresiduals1)

data(stresiduals2)
summary(stresiduals2)
}

