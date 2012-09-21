\name{summary.thinresid}
\alias{summary.thinresid}
\title{
Summary of a set of thinned residuals
}
\description{
Outputs and prints a summary of a set of thinned residuals.
}
\usage{
summary.thinresid(X)
}
\arguments{
  \item{X}{
A \code{\dQuote{thinresid}} object.
}
}
\details{
Outputs and prints a summary of \code{X}.

Printed to the screen are the thinning rate, \code{k}, the number of thinned residuals, \code{n}, the expected number of residuals, \code{n.exp}, and the p-value for observing \code{n} residuals (\code{p.val}).
}
\value{
A list of 
 	\item{k }{ The thinning rate.}
 	\item{n }{ The number of residuals.}
 	\item{n.exp }{ The expected number of residuals.}
 	\item{p.val }{ The p-value for \code{n}.}
}
\author{
Robert Clements
}
\seealso{
\code{\link{thinresid}}
}
\examples{
data(tresiduals1)
summary(tresiduals1)

data(tresiduals2)
summary(tresiduals2)
}
