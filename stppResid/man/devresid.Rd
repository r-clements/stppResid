\name{devresid}
\alias{devresid}
\title{
Calculate deviance residuals
}
\description{
\code{devresid} divides the space-time window into a grid of bins and calculates the deviance residuals within each bin between two competing conditional intensity models.
}
\usage{
devresid(X, cifunction1, cifunction2, theta1 = NULL, theta2 = NULL, 
lambda1 = NULL, lambda2 = NULL, grid = c(10, 10), gf = NULL, algthm1 = 
c("cubature", "mc", "miser", "none"), algthm2 = c("cubature", "mc", "miser", "none"), 
n = 100, n1.miser = 10000, n2.miser = 10000, tol = 1e-05, maxEval = 0, 
absError = 0, ints1 = NULL, ints2 = NULL)
}
\arguments{
  \item{X}{
A \dQuote{\code{stpp}} object.
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
  \item{grid}{
A vector representing the number of columns and rows in the grid.
}
  \item{gf}{
Optional: A \dQuote{\code{stgrid}} object.
}
  \item{algthm1}{
The algorithm used for estimating the integrals in each grid cell for model 1. The three algorithms are \dQuote{\code{cubature}}, 
\dQuote{\code{mc}}, \dQuote{\code{miser}}, and \dQuote{\code{none}}.  
}
    \item{algthm2}{
The algorithm used for estimating the integrals in each grid cell for model 2. The three algorithms are \dQuote{\code{cubature}}, 
\dQuote{\code{mc}}, \dQuote{\code{miser}}, and \dQuote{\code{none}}.  
}
  \item{n}{
Initial number of sample points in each grid cell for approximating integrals. The number of sample points are iteratively increased by \code{n} until some accuracy threshold is reached.
}
  \item{n1.miser}{
The total number of sample points for estimating all integrals for model 1 if the \code{miser} algorithm is selected.  
}
  \item{n2.miser}{
The total number of sample points for estimating all integrals for model 2 if the \code{miser} algorithm is selected.  
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
  \item{ints1}{
An optional vector of integrals for model 1. Must be the same length as the number of rows in \code{grid}, and each element of \code{ints1} should correspond to each row in \code{grid}.  
}
\item{ints2}{
An optional vector of integrals for model 2. Must be the same length as the number of rows in \code{grid}, and each element of \code{ints2} should correspond to each row in \code{grid}.  
}
}
\details{
The deviance residuals are the differences in the log-likelihoods of model 1 vs. model 2 within each space-time bin, denoted here as \eqn{B_{i}}{B_i} (see Wong and Schoenberg (2010)). The deviance residual is given by 

\deqn{R_{D}(B_i) = \sum_{i:(x_{i})\in B_{i}} log \hat{\lambda}_{1}(x_{i}) - \int_{B_{i}} \hat{\lambda}_{1}(x_{i}) dx - \left(\sum_{i:(x_{i})\in B_{i}} log \hat{\lambda}_{2}(x_{i}) - \int_{B_{i}} \hat{\lambda}_{2}(x_{i}) dx\right),}
	
where \eqn{\hat{\lambda}(x)}{lambda_hat} is the fitted conditional intensity model.

The conditional intensity functions, \code{cifunction1} and \code{cifunction2}, should take \code{X} as their first argument, and an optional \code{theta} as their second argument, and return a vector of conditional intensity estimates with length equal to the number of points in \code{X}, i.e. the length of \code{X$x}. Both \code{cifunction1} and \code{cifunction2} are required. \code{lambda1} and \code{lambda2} are optional, and if passed will eliminate the need for \code{devresid} to calculate the conditional intensities at each observed point in \code{X}.

The integrals in \eqn{R(B_{i})} are approximated using one of three algorithms: the \code{adaptIntegrate} function from the \code{cubature} pakcage, a simple Monte Carlo (\code{mc}) algorithm, or the \code{miser} algorithm. The default is \code{cubature} and should be the fastest approximation. The approximation continues until either the maximum number of evaluations is reached, the error is less than the absolute error, or is less than the tolerance times the integral.

The simple Monte Carlo iteratively adds \code{n} sample points to each grid cell to approximate the integral, and the iteration stops when some threshold in the accuracy of the approximation is reached. The MISER algorithm samples a total number of \code{n1.miser} and/or \code{n2.miser} points in a recursive way, sampling the points in locations that have the highest variance. This part can be very slow and the approximations can be very inaccurate. For highest accuracy these algorithms will require a very large \code{n} or \code{n1.miser}/\code{n2.miser} depending on the complexity of the conditional intensity functions (some might say ~1 billion sample points are needed for a good approximation). Passing the argument \code{ints1} and/or \code{ints2} eliminates the need for approximating the integrals using either of these two algorithms. 

Passing \code{gf} will eliminate the need for \code{devresid} to create a \dQuote{\code{stgrid}} object. If neither \code{grid} or \code{gf} is specified, the default \code{grid} is 10 by 10. 
}
\value{
Prints to the screen the number of simulated points used to approximate the integrals.

Outputs an object of class \dQuote{\code{devresid}}, which is a list of
	\item{X }{ An object of class \dQuote{\code{stpp}}.}
	\item{grid }{ An object of class \dQuote{\code{stgrid}}.}
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
data(simdata)
X <- stpp(simdata$x, simdata$y, simdata$t)

#===> define two conditional intensity functions <===#
ci1 <- function(X, theta){theta*exp(-2*X$x - 2*X$y - 2*X$t)} #correct model

ci2 <- function(X, theta = NULL){rep(250, length(X$x))} #homogeneous Poisson model

deviance <- devresid(X, ci1, ci2, theta1 = 3000)

#===> plot results <===#
plot(deviance)
}