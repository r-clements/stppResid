\name{gridresid}
\alias{gridresid}
\title{
Calculate grid-based residuals
}
\description{
\code{gridresid} divides the space-time window into a grid of bins and calculates residuals within each bin for a specified conditional intensity model.
}
\usage{
gridresid(X, cifunction, theta = NULL, lambda = NULL, grid = c(10, 10), gf = NULL, resid = c("raw", "pearson", "inverse"), algthm = c("cubature", "mc", "miser", "none"), n = 100, n.miser = 10000, tol = 1e-05, maxEval = 0, absError = 0, ints = NULL)
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
  \item{lambda}{
Optional: A vector of conditional intensities at each point in \code{X}.
}
  \item{grid}{
A vector representing the number of columns and rows in the grid.
}
  \item{gf}{
Optional: A \dQuote{\code{stgrid}} object.
}
  \item{resid}{
The residual type to be computed. The three types are \dQuote{\code{raw}}, \dQuote{\code{pearson}}, and \dQuote{\code{inverse}}.
}
  \item{algthm}{
The algorithm used for estimating the integrals in each grid cell. The three algorithms are \dQuote{\code{cubature}}, \dQuote{\code{mc}}, \dQuote{\code{miser}}, and \dQuote{\code{none}}  
}
  \item{n}{
Initial number of sample points in each grid cell for approximating integrals. The number of sample points are iteratively increased by \code{n} until some accuracy threshold is reached.
}
  \item{n.miser}{
The total number of sample points for estimating all integrals.  
}
  \item{tol}{
The maximum tolerance.
}
  \item{maxEval}{
The maximum number of function evaluations needed (default 0 implies no limit).  
}
  \item{absError}{
The maximum absolute error tolerated.  
}
  \item{ints}{
An optional vector of integrals. Must be the same length as the number of rows in \code{grid}, and each element of \code{ints} should correspond to each row in \code{grid}.  
}
}
\details{
The grid-based residuals are well known residuals for temporal point processes, and purely spatial point processes (see Baddeley et al. (2005)), extended to space-time point processes in Clements et al. (2010). They consist of the \code{raw} residual, and rescaled versions of the \code{raw} residual called the \code{pearson} residual and the \code{inverse} residual.

The \code{raw} residual for bin i (\eqn{B_{i}}{B_i}) is defined as the number of points in \eqn{B_{i}}{B_i} minus the expected number of points in \eqn{B_{i}}{B_i},

\deqn{R(B_{i}) = N(B_{i}) - \int_{B_{i}} \hat{\lambda}(x) dx,}

where \eqn{\hat{\lambda}(x)}{lambda_hat(x)} is the fitted conditional intesity model.

The \code{pearson} residual is defined as

\deqn{R_{p}(B_{i}) = \sum_{x_{i} \in B_{i}}1/\sqrt{\hat{\lambda}(x_{i})} - \int_{B_{i}}\sqrt{\hat{\lambda}(x)} dx.}

The \code{inverse} residual is defined as

\deqn{R_{I}(B_{i}) = \sum_{x_{i} \in B_{i}}1/\hat{\lambda}(x_{i}) - \int_{B_{i}}I(\hat{\lambda}(x) > 0)dx.}

If neither type of residual is specified, the default residual to be computed is the \code{raw} residual.

The conditional intensity function, \code{cifunction}, should take \code{X} as the first argument, and an optional \code{theta} as the second argument, and return a vector of conditional intensity estimates with length equal to the number of points in \code{X}, i.e. the length of \code{X$x}. \code{cifunction} is required, while \code{lambda} is optional. \code{lambda} eliminates the need for \code{gridresid} to calculate the conditional intensity at each observed point in \code{X}.

The integrals in \eqn{R(B_{i})} are approximated using one of three algorithms: the \code{adaptIntegrate} function from the \code{cubature} pakcage, a simple Monte Carlo (\code{mc}) algorithm, or the \code{miser} algorithm. The default is \code{cubature} and should be the fastest approximation. The approximation continues until either the maximum number of evaluations is reached, the error is less than the absolute error, or is less than the tolerance times the integral.

The simple Monte Carlo iteratively adds \code{n} sample points to each grid cell to approximate the integral, and the iteration stops when some threshold in the accuracy of the approximation is reached. The MISER algorithm samples a total number of \code{n.miser} points in a recursive way, sampling the points in locations that have the highest variance. This part can be very slow and the approximations can be very inaccurate. For highest accuracy these algorithms will require a very large \code{n} or \code{n.miser} depending on the complexity of the conditional intensity functions (some might say ~1 billion sample points are needed for a good approximation). Passing the argument \code{ints} eliminates the need for approximating the integrals using either of these two algorithms. 

Passing \code{gf} will eliminate the need for \code{gridresid} to create a \dQuote{\code{stgrid}} object. If neither \code{grid} or \code{gf} is specified, the default \code{grid} is 10 by 10. 
}
\value{
Prints to the screen the number of simulated points in each bin used to approximate the integrals.	
	
Outputs an object of class \dQuote{\code{gridresid}}, which is a list of
	\item{X }{ An object of class \dQuote{\code{stpp}}.}
	\item{grid }{ An object of class \dQuote{\code{stgrid}}.}
	\item{residuals }{ A vector of grid-based residuals. The order of the residuals corresponds with the order of the bins in \code{grid}.}
  \item{n}{Total number of points used for approximating all integrals.}
  \item{integral}{Vector of actual integral approximations in each grid cell.}
  \item{mean.lambda}{Vector of the approximate final mean of lambda in each grid cell.}
  \item{sd.lambda}{Vector of the approximate standard deviation of lambda in each grid cell.}
If the \code{miser} algorithm is selected, the following is also returned:
  \item{app.pts}{A data frame of the x,y, and t coordinates of a sample of 10,000 of the sampled points for integral approximation, along with the value of lambda (l).}
}
\references{
Baddeley, A., Turner, R.,Moller, J., and Hazelton, M. (2005) Residual analysis for spatial point processes. \emph{Journal of the Royal Statistical Society,} \bold{67}, 617--666.

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
data(simdata)
X <- stpp(simdata$x, simdata$y, simdata$t)

#===> define two conditional intensity functions <===#
ci1 <- function(X, theta){theta*exp(-2*X$x - 2*X$y - 2*X$t)} #correct model

ci2 <- function(X, theta = NULL){rep(250, length(X$x))} #homogeneous Poisson model

gresiduals <- gridresid(X, ci1, theta = 3000)
gresiduals2 <- gridresid(X, ci2)

#===> plot results <===#
plot(gresiduals)
plot(gresiduals2)
}