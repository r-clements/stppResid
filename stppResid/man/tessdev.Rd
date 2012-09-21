\name{tessdev}
\alias{tessdev}
\alias{print.tessdev}
\alias{plot.tessdev}
\title{
Calculate tessellation deviance residuals
}
\description{
\code{tessdev} divides the space-time window into cells using a Voronoi tessellation and calculates the deviance residuals within each cell between two competing conditional intensity models.
}
\usage{
tessdev(X, cifunction1, cifunction2, theta1 = NULL, theta2 = NULL, lambda1 = NULL, lambda2 = NULL, algthm1 = c(\dQuote{mc}, \dQuote{miser}, \dQuote{none}), algthm2 = c(\dQuote{mc}, \dQuote{miser}, \dQuote{none}), n = 100, n1.miser = 10000, n2.miser = 10000, ints1 = NULL, ints2 = NULL)
}
\arguments{
  \item{X}{
A \code{\dQuote{stpp}} object.
}
  \item{cifunction1}{
A function returning estimates of the conditional intensity at all points in \code{X}, according to model 1 (\code{cifunction1}). The function should take arguments \code{X} and an optional vector of parameters \code{theta1}.
}
  \item{cifunction2}{
A function returning estimates of the conditional intensity at all points in \code{X}, according to model 2 (\code{cifunction2}) which should be different than model 1 (\code{cifunction1}). The function should take arguments \code{X} and an optional vector of parameters \code{theta2}.
}
  \item{theta1}{
Optional: A vector of parameters to be passed to \code{cifunction1}.
}
  \item{theta2}{
Optional: A vector of parameters to be passed to \code{cifunction2}.
}
  \item{lambda1}{
Optional: A vector of conditional intensities based on \code{cifunction1} at each point in \code{X}.
}
  \item{lambda2}{
Optional: A vector of conditional intensities based on \code{cifunction2} at each point in \code{X}.
}
  \item{algthm1}{
The algorithm used for estimating the integrals in each cell for model 1. The three algorithms are \code{\dQuote{mc}}, \code{\dQuote{miser}}, and \code{\dQuote{none}}  
}
    \item{algthm2}{
The algorithm used for estimating the integrals in each cell for model 2. The three algorithms are \code{\dQuote{mc}}, \code{\dQuote{miser}}, and \code{\dQuote{none}}  
}
  \item{n}{
Initial number of sample points in each cell for approximating integrals. The number of sample points are iteratively increased by \code{n} until some accuracy threshold is reached.
}
  \item{n1.miser}{
The total number of sample points for estimating all integrals for model 1 if the \code{miser} algorithm is selected.  
}
  \item{n2.miser}{
The total number of sample points for estimating all integrals for model 2 if the \code{miser} algorithm is selected.  
}
  \item{ints1}{
An optional vector of integrals for model 1. Must be the same length as the number of tessellation cells, and each element of \code{ints1} should correspond to each cell in the \code{tile.list} that is returned using the \code{deldir} function, which can be called separately.  
}
\item{ints2}{
An optional vector of integrals for model 2. Must be the same length as the number of tessellation cells, and each element of \code{ints2} should correspond to each cell in the \code{tile.list} that is returned using the \code{deldir} function, which can be called separately.  
}
}
\details{
The tessellation deviance residuals are the differences in the tessellation residuals of model 1 vs. model 2 within each Voronoi tessellation cell, denoted here as \eqn{V_{i}}{V_i}. The tessellation deviance residual is given by 

\deqn{R_{TD}(V_{i}) = \left(1 - \int_{V_{i}}\hat{\lambda}_{1}(x)dx\right)/\sqrt{\int_{V_{i}}\hat{\lambda}_{1}(x)dx}-\left(1 - \int_{V_{i}}\hat{\lambda}_{2}(x)dx\right)/\sqrt{\int_{V_{i}}\hat{\lambda}_{2}(x)dx},}
	
where \eqn{\hat{\lambda}(x)}{lambda_hat} is the fitted conditional intensity model.

The conditional intensity functions, \code{cifunction1} and \code{cifunction2}, should take \code{X} as their first argument, and an optional \code{theta} as their second argument, and return a vector of conditional intensity estimates with length equal to the number of points in \code{X}, i.e. the length of \code{X$x}. Both \code{cifunction1} and \code{cifunction2} are required. \code{lambda1} and \code{lambda2} are optional, and if passed will eliminate the need for \code{devresid} to calculate the conditional intensities at each observed point in \code{X}.

The integrals in \eqn{R_{TD}(V_{i})} are approximated using one of two algorithms: a simple Monte Carlo (\code{mc}) algorithm, or the MISER algorithm. The simple Monte Carlo iteratively adds \code{n} sample points to each tessellation cell to approximate the integral, and the iteration stops when some threshold in the accuracy of the approximation is reached. The MISER algorithm samples a total number of \code{n.miser} points in a recursive way, sampling the points in locations that have the highest variance. This part can be very slow and the approximations can be very inaccurate. For highest accuracy these algorithms will require a very large \code{n} or \code{n.miser} depending on the complexity of the conditional intensity functions (some might say ~1 billion sample points are needed for a good approximation). 

Passing the arguments \code{ints1} and/or \code{ints2} eliminates the need for approximating the integrals using either of the two algorithms here. However, the \code{tile.list} must first be obtained in order to assure that each element of \code{ints1} and/or \code{ints2} corresponds to the correct cell. The \code{tile.list} can be obtained, either by using the \code{deldir} function separately, or by using \code{tessresid} with one of the included algorithms first (the \code{tile.list} is returned along with the residuals). \code{tessresid} can then be called again with \code{ints1} and/or \code{ints2} included and \code{algthm = \dQuote{none}}.

Note that if \code{miser} is selected, and if the points in the point pattern are very densely clustered, the integral in some cells may end up being approximated based on only the observed point in the point pattern that is contained in that cell. This happens because the cells in these clusters of points will be very small, and so it may be likely that sampled points based on the MISER algorithm will miss these cells entirely. For this reason, the simple Monte Carlo algorithm might be preferred. 
}
\value{
Prints to the screen the number of simulated points used to approximate the integrals.

Outputs an object of class \code{\dQuote{devresid}}, which is a list of
	\item{X }{ An object of class \code{\dQuote{stpp}}.}
	\item{grid }{ An object of class \code{\dQuote{stgrid}}.}
	\item{residuals }{ A vector of deviance residuals. The order of the residuals corresponds with the order of the bins in \code{grid}.}
The following elements are named by model number, e.g. n.1, n.2, integral.1, integral.2, etc..
  \item{n}{Total number of points used for approximating all integrals.}
  \item{integral}{Vector of actual integral approximations in each grid cell.}
  \item{mean.lambda}{Vector of the approximate final mean of lambda in each grid cell.}
  \item{sd.lambda}{Vector of the approximate standard deviation of lambda in each grid cell.}
If the \code{miser} algorithm is selected, the following is also returned:
  \item{app.pts}{A data frame of the x,y, and t coordinates of a sample of 10,000 of the sampled points for integral approximation, along with the value of lambda (l).}
}
\references{
Wong, K., Schoenberg, F.P. "On mainshock focal mechanisms and the spatial distribution of aftershocks",  Bulletin of the Seismological Society of America, In review.

Clements, R.A., Schoenberg, F.P., and Schorlemmer, D. (2011) Residual analysis methods for space-time point processes with applications to earthquake forecast models in California. \emph{Annals of Applied Statistics,} \bold{5}, Number 4, 2549--2571.
}
\author{
Robert Clements
}
\seealso{
\code{\link{make.grid}}
}
\examples{
#===> load simulated data <===#
load(simdata)
X <- stpp(simdata$x, simdata$y, simdata$t)

#===> define two conditional intensity functions <===#
ci1 <- function(X, theta){theta*exp(-2*X$x - 2*X$y - 2*X$t)} #correct model

ci2 <- function(X, theta = NULL){rep(250, length(X$x))} #homogeneous Poisson model

deviance <- devresid(X, ci1, ci2, theta1 = 3000)

#===> plot results <===#
plot(deviance)
}