\name{supresid}
\alias{supresid}
\title{
Perform superposed residuals method
}
\description{
\code{supresid} takes a space-time point pattern and conditional intensity model and calculates a set of superposed residuals for further analysis.
}
\usage{
supresid(X, cifunction, theta = NULL, k = NULL, lambda = NULL)
}
\arguments{
  \item{X}{
A \dQuote{\code{stpp}} object.
}
  \item{cifunction}{
A function returning the value of the conditional intensity at all points in \code{X}. The function should take arguments \code{X} and an optional vector of parameters \code{theta}.
}
  \item{theta}{
Optional: A vector of parameters to be passed to \code{cifunction}.
}
  \item{k}{
The superposition rate.
}
  \item{lambda}{
Optional: A vector of conditional intensities at each point in \code{X}.
}
}
\details{
Superposed residuals is a type of transformation based residuals for space-time point processes (see Bremaud (1981)) which consists of superimposing a point process with rate \code{k} - \eqn{\hat{\lambda}}{lambda_hat} onto the observed point process. \code{k} should be the maximum conditional intensity over the entire space-time window. If the model for the conditional intensity is correct, the residuals should be homogeneous Poisson with rate \code{k}. Any patterns or inter-point interaction in the residuals indicates a lack of fit of the model. To test for homogeneity, a commonly used tool is Ripley's K-function, a version of which can be found in the \code{spatstat} package.

The conditional intensity function, \code{cifunction}, should take \code{X} as the first argument, and an optional \code{theta} as the second argument, and return a vector of conditional intensity estimates with length equal to the number of points in \code{X}, i.e. the length of \code{X$x}. \code{cifunction} is required, while \code{lambda} is optional. \code{lambda} eliminates the need for \code{supresid} to calculate the conditional intensity at each observed point in \code{X}.

If \code{k} is not specified, the default is the maximum \eqn{\hat{\lambda}}{lambda_hat} estimated at the points.
}
\value{
Outputs an object of class \dQuote{\code{supresid}}, which is a list of
	\item{X }{ An object of class \dQuote{\code{stpp}}.}
	\item{k }{ The superposition rate.}
	\item{residuals }{ A data frame consisting of the x, y, and t coordinates of the superposed residuals.}
	\item{super }{ A data frame consisting of the x, y, and t coordinates of the superposed points.}
}
\references{
Bremaud, P. \emph{Point Processes and Queues: Martingale Dynamics.} SpringerVerlag, New York, 1981.

Clements, R.A., Schoenberg, F.P., and Schorlemmer, D. (2011) Residual analysis methods for space-time point processes with applications to earthquake forecast models in California. \emph{Annals of Applied Statistics,} \bold{5}, Number 4, 2549--2571.
}
\author{
Robert Clements
}
\seealso{
\code{\link{stpp}}, \code{\link{thinresid}}, \code{\link{superthin}}
}
\examples{
#===> load simulated data <===#
data(simdata)
X <- stpp(simdata$x, simdata$y, simdata$t)

#===> define conditional intensity function <===#
ci1 <- function(X, theta){theta[1]*exp(-theta[2]*X$x - 
theta[3]*X$y - theta[4]*X$t)} #correct model

sresiduals1 <- supresid(X, ci1, theta = c(3000, 2, 2, 2))
sresiduals2 <- supresid(X, ci1, theta = c(2500, 5, 5, 10))
#===> plot results <===#
par(mfrow = c(1,2))
plot(sresiduals1)
plot(sresiduals2)

summary(sresiduals1)
summary(sresiduals2)
}