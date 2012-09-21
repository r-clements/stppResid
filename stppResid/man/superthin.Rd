\name{superthin}
\alias{superthin}
\alias{print.superthin}
\alias{plot.superthin}
\alias{summary.superthin}
\title{
Perform super-thinned residuals method
}
\description{
\code{superthin} takes a space-time point pattern and conditional intensity model and calculates a set of super-thinned residuals for further analysis.
}
\usage{
superthin(X, cifunction, theta = NULL, k = NULL, lambda = NULL)
}
\arguments{
  \item{X}{
A \code{\dQuote{stpp}} object.
}
  \item{cifunction}{
A function returning the value of the conditional intensity at all points in \code{X}. The function should take arguments \code{X} and an optional vector of parameters \code{theta}.
}
  \item{theta}{
Optional: A vector of parameters to be passed to \code{cifunction}.
}
  \item{k}{
The super-thinning rate.
}
  \item{lambda}{
Optional: A vector of conditional intensities at each point in \code{X}.
}
}
\details{
Super-thinned residuals (Clements et. al. (2012)) is a type of transformation based residuals for space-time point processes based on both thinned residuals (see Schoenberg (2003)) and superposed residuals (see Bremaud (1981)). The residuals consist of a set of points that should be homogeneous Poisson, with rate \code{k}, if the model for the conditional intensity is correct. Any patterns or inter-point interaction in the residuals indicates a lack of fit of the model. To test for homogeneity, a commonly used tool is Ripley's K-function, a version of which can be found in the \code{\link{spatstat}} package. 

Super-thinned residuals are found as follows:

1. The super-thinning rate \code{k} is specified. This rate determines the amount of thinning and superposition conducted, and also determines the final rate of the super-thinned residual point process.

2. All observed points in \code{X} where \eqn{\hat{\lambda} <}{lambda_hat <} \code{k} are automatically kept.

3. All points in \code{X} where \eqn{\hat{\lambda} >=}{lambda_hat >=} \code{k} are kept with probability \code{k}\eqn{/\hat{\lambda}}{/lambda_hat}.

4. In all space-time locations where \eqn{\lambda <} \code{k}, points are simulated with rate \code{k} \eqn{- \hat{\lambda}}{- lambda_hat}.

The result should be a homogeneous Poisson process with rate \code{k} if the model is correct.

The conditional intensity function, \code{cifunction}, should take \code{X} as the first argument, and an optional \code{theta} as the second argument, and return a vector of conditional intensity estimates with length equal to the number of points in \code{X}, i.e. the length of \code{X$x}. \code{cifunction} is required, while \code{lambda} is optional. \code{lambda} eliminates the need for \code{superthin} to calculate the conditional intensity at each observed point in \code{X}.

If \code{k} is not specified, the default is the mean of \eqn{\hat{\lambda}}{lambda_hat} estimated by the total number of points divided by the volume of the space-time window.
}
\value{
Outputs an object of class \code{\dQuote{superthin}}, which is a list of
	\item{X }{ An object of class \code{\dQuote{stpp}}.}
	\item{k }{ The super-thinning rate.}
	\item{residuals }{ A data frame consisting of the x, y, and t coordinates of the super-thinned residuals.}
	\item{super }{ A data frame consisting of the x, y, and t coordinates of the superposed points.}
	\item{keep1 }{ A data frame consisting of the x, y, and t coordinates of the automatically retained points.}
	\item{keep2 }{ A data frame consisting of the x, y, and t coordinates of the points remaining after the thinning has taken place.}
	\item{deleted }{ A data frame consisting of the x, y, and t coordinates of the points removed during the thinning process.}
}
\references{
Bremaud, P. \emph{Point Processes and Queues: Martingale Dynamics.} SpringerVerlag, New York, 1981.

Clements, R.A., Schoenberg, F.P., and Veen, A. (2012) Evaluation of space-time point process models using super-thinning. \emph{Environmetrics}, to appear.

Schoenberg, F.P. (2003) Multi-dimensional residuals analysis of point process models for earthquake occurrences. \emph{Journal of the American Statistical Association,} \bold{98}, 789--795.

Clements, R.A., Schoenberg, F.P., and Schorlemmer, D. (2011) Residual analysis methods for space-time point processes with applications to earthquake forecast models in California. \emph{Annals of Applied Statistics,} \bold{5}, Number 4, 2549--2571.
}
\author{
Robert Clements
}
\seealso{
\code{\link{stpp}}, \code{\link{thinresid}}, \code{\link{supresid}}, \code{\link{spatstat}}
}
\examples{
#===> load simulated data <===#
load(simdata)
X <- stpp(simdata$x, simdata$y, simdata$t)

#===> define conditional intensity function <===#
ci1 <- function(X, theta){theta[1]*exp(-theta[2]*X$x - theta[3]*X$y - theta[4]*X$t)} #correct model

stresiduals1 <- superthin(X, ci1, theta = c(3000, 2, 2, 2), k = 250)
stresiduals2 <- superthin(X, ci1, theta = c(2500, 5, 5, 10), k = 250)
#===> plot results <===#
par(mfrow = c(1,2))
plot(stresiduals1)
plot(stresiduals2)

summary(stresiduals1)
summary(stresiduals2)
}