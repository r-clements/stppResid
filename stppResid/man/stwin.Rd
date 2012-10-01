\name{stwin}
\alias{stwin}
\title{
Create a space-time window
}
\description{
\code{stwin} creates an object of class \dQuote{\code{stwin}} representing a three-dimensional space-time window.
}
\usage{
stwin(xcoord = c(0, 1), ycoord = c(0, 1), tcoord = c(0, 1))
}
\arguments{
  \item{xcoord}{
A vector of x coordinate limits. 
}
  \item{ycoord}{
A vector of y coordinate limits.
}
  \item{tcoord}{
A vector of t coordinate limits.
}
}
\details{
To create a space-time point process object of class \dQuote{\code{stpp}}, an enclosing space-time window of class \dQuote{\code{stwin}} must be created and passed to \code{\link{stpp}}.

The window must be box shaped. Each vector of coordinates must be of length two and ordered from smallest to largest. Every combination of the entries of the three vectors represents the 8 corners of the space-time window.

If no coordinates are given, the default is a unit cube.
}
\value{
Outputs an object of class \dQuote{\code{stwin}} describing a three-dimensional space-time cuboid, which is a list of
	\item{xcoord }{ A vector of x limits.}
	\item{ycoord }{ A vector of y limits.}
	\item{tcoord }{ A vector of t limits.}
}
\author{
Robert Clements
}
\seealso{
\code{\link{stpp}}
}
\examples{
#===> create a stpp object <===#
x <- rnorm(30, mean = 10, sd = 1)
y <- rnorm(30, mean = 100, sd = 10)
t <- runif(30, 0, 100)
stw <- stwin(xcoord = c(0, 20), ycoord = c(50, 150), tcoord = c(0, 100))
X <- stpp(x, y, t, stw = stw)

#===> create a stpp object from redbanana data <===#
data(redbanana)
attach(redbanana)
xcoord <- c(min(longitude)-.01, max(longitude)+.01)
ycoord <- c(min(latitude)-.01, max(latitude)+.01)
tcoord <- c(0, max(birth)+.01)
stw <- stwin(xcoord, ycoord, tcoord)
X <- stpp(longitude, latitude, birth, stw)
}