\name{tessresid}
\alias{tessresid}
\title{
Calculate tessellation residuals
}
\description{
\code{tessresid} divides the space-time window into bins using a Voronoi tessellation and calculates residuals within each bin for a specified conditional intensity model.
}
\usage{
tessresid(X, cifunction, theta = NULL, algthm = c("mc", "miser", "none"), 
n = 100, n.miser = 10000, ints = NULL)
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
  \item{algthm}{
The algorithm used for estimating the integrals in each tessellation cell. The three algorithms are \dQuote{\code{mc}}, \dQuote{\code{miser}}, and \dQuote{\code{none}}  
}
  \item{n}{
Initial number of sample points in each grid tessellation for approximating integrals. The number of sample points are iteratively increased by \code{n} until some accuracy threshold is reached.
}
  \item{n.miser}{
The total number of sample points for estimating all integrals.  
}
  \item{ints}{
An optional vector of integrals. Must be the same length as the number of tessellation cells, and each element of \code{ints} should correspond to each cell in the \code{tile.list} that is returned using the \code{deldir} function, which can be called separately.  
}
}
\details{
Tessellation residuals are residuals calculated in bins that are created by dividing up the spatial window using a Voronoi tessellation. Because the bins are based on a tessellation, each bin contains at most one point. The residual in bin i (\eqn{V_{i}}{V_i}) is defined by

\deqn{R_{T}(V_{i}) = \left(1 - \int_{V_{i}}\hat{\lambda}(x)dx\right)/\sqrt{\int_{V_{i}}\hat{\lambda}(x)dx},}

where \eqn{\hat{\lambda}(x)}{lambda_hat(x)} is the fitted conditional intensity model. 

The conditional intensity function, \code{cifunction}, should take \code{X} as the first argument, and an optional \code{theta} as the second argument, and return a vector of conditional intensity estimates with length equal to the number of points in \code{X}, i.e. the length of \code{X$x}. \code{cifunction} is required, while \code{lambda} is optional. \code{lambda} eliminates the need for \code{tessresid} to calculate the conditional intensity at each observed point in \code{X}.

The integrals in \eqn{R_{T}(V_{i})} are approximated using one of two algorithms: a simple Monte Carlo (\code{mc}) algorithm, or the MISER algorithm. The simple Monte Carlo iteratively adds \code{n} sample points to each tessellation cell to approximate the integral, and the iteration stops when some threshold in the accuracy of the approximation is reached. The MISER algorithm samples a total number of \code{n.miser} points in a recursive way, sampling the points in locations that have the highest variance. This part can be very slow and the approximations can be very inaccurate. For highest accuracy these algorithms will require a very large \code{n} or \code{n.miser} depending on the complexity of the conditional intensity functions (some might say ~1 billion sample points are needed for a good approximation). 

Passing the argument \code{ints} eliminates the need for approximating the integrals using either of the two algorithms here. However, the \code{tile.list} must first be obtained in order to assure that each element of \code{ints} corresponds to the correct cell. The \code{tile.list} can be obtained, either by using the \code{deldir} function separately, or by using \code{tessresid} with one of the included algorithms first (the \code{tile.list} is returned along with the residuals). \code{tessresid} can then be called again with \code{ints} included and \code{algthm} = \dQuote{\code{none}}.

Note that if \code{miser} is selected, and if the points in the point pattern are very densely clustered, the integral in some cells may end up being approximated based on only the observed point in the point pattern that is contained in that cell. This happens because the cells in these clusters of points will be very small, and so it may be likely that sampled points based on the MISER algorithm will miss these cells entirely. For this reason, the simple Monte Carlo algorithm might be preferred. 
}
\value{
Outputs an object of class \dQuote{\code{tessresid}}, which is a list of
	\item{X }{ An object of class \dQuote{\code{stpp}}.}
	\item{tile.list }{ An object of type \dQuote{\code{tile.list}}, which is itself a list with one entry for each point in \code{X}. Each entry is a list of
  
pt: x and y coordinates of the point.

x: x coordinates of the vertices of the tessellation cell.
    
y: y coordinates of the vertices of the tessellation cell.} 
	\item{residuals }{ A vector of tessellation residuals. The order of the residuals corresponds with the order of the cells in \code{tile.list}.}
}
\author{
Robert Clements
}
\seealso{
\code{\link{gridresid}}, \code{\link{deldir}}, \code{\link{tile.list}}
}
\examples{
#===> load simulated data <===#
data(simdata)
X <- stpp(simdata$x, simdata$y, simdata$t)

#===> define two conditional intensity functions <===#
ci1 <- function(X, theta){theta*exp(-2*X$x - 2*X$y - 2*X$t)} #correct model

ci2 <- function(X, theta = NULL){rep(250, length(X$x))} #homogeneous Poisson model

tsresiduals <- tessresid(X, ci1, theta = 3000)
tsresiduals2 <- tessresid(X, ci2)
#===> plot results <===#
plot(tsresiduals)
plot(tsresiduals2)
}