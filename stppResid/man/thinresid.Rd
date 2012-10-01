\name{thinresid}
\alias{thinresid}
\title{
Perform thinned residuals method
}
\description{
\code{thinresid} takes a space-time point pattern and conditional intensity model and calculates a set of thinned residuals for further analysis.
}
\usage{
thinresid(X, cifunction = NULL, theta = NULL, k = NULL, lambda = NULL)
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
The thinning rate.
}
  \item{lambda}{
Optional: A vector of conditional intensities at each point in \code{X}.
}
}
\details{
Thinned residuals is a type of transformation based residuals for space-time point processes (see Schoenberg (2003)) which consists of thinning out the observed points using the fitted conditional intensity model, \eqn{\hat{\lambda}}{lambda_hat}.  Each point is kept with probability \code{k}\eqn{/\hat{\lambda}}{/lambda_hat}, where \code{k} should be the minimum conditional intensity over the entire space-time window.  If the model for the conditional intensity is correct, the residuals should be homogeneous Poisson with rate \code{k}.  Any patterns or inter-point interaction in the residuals indicates a lack of fit of the model.  To test for homogeneity, a commonly used tool is Ripley's K-function, a version of which can be found in the \code{spatstat} package.

The conditional intensity function, \code{cifunction}, should take \code{X} as the first argument, and an optional \code{theta} as the second argument, and return a vector of conditional intensity estimates with length equal to the number of points in \code{X}, i.e. the length of \code{X$x}.  \code{cifunction} is required, while \code{lambda} is optional.  \code{lambda} eliminates the need for \code{thinresid} to calculate the conditional intensity at each observed point in \code{X}.

If \code{k} is not specified, the default is the minimum \eqn{\hat{\lambda}}{lambda_hat} estimated at the points.
}
\value{
Outputs an object of class \dQuote{\code{thinresid}}, which is a list of
	\item{X }{ An object of class \dQuote{\code{stpp}}.}
	\item{k }{ The thinning rate.}
	\item{residuals }{ A data frame consisting of the x, y, and t coordinates of the thinned residuals.}
	\item{deleted }{ A data frame consisting of the x, y, and t coordinates of the points removed during the thinning process.}
}
\references{
Schoenberg, F.P. (2003) Multi-dimensional residuals analysis of point process models for earthquake occurrences. \emph{Journal of the American Statistical Association,} \bold{98}, 789--795.

Clements, R.A., Schoenberg, F.P., and Schorlemmer, D. (2011) Residual analysis methods for space-time point processes with applications to earthquake forecast models in California. \emph{Annals of Applied Statistics,} \bold{5}, Number 4, 2549--2571.
}
\author{
Robert Clements
}
\seealso{
\code{\link{stpp}}, \code{\link{supresid}}, \code{\link{superthin}}
}
\examples{
#===> load simulated data <===#
data(simdata)
X <- stpp(simdata$x, simdata$y, simdata$t)

#===> define conditional intensity function <===#
ci1 <- function(X, theta){theta[1]*exp(-theta[2]*X$x - 
theta[3]*X$y - theta[4]*X$t)} #correct model

tresiduals1 <- thinresid(X, ci1, theta = c(3000, 2, 2, 2))
tresiduals2 <- thinresid(X, ci1, theta = c(2500, 5, 5, 10))
#===> plot results <===#
par(mfrow = c(1,2))
plot(tresiduals1)
plot(tresiduals2)

summary(tresiduals1)
summary(tresiduals2)
}