\name{stpp}
\alias{stpp}
\alias{print.stpp}
\alias{plot.stpp}
\title{
Convert data to class \code{stpp}
}
\description{
\code{stpp} creates a space-time point pattern of class \code{\dQuote{stpp}} for use by the package \code{stppResid}.
}
\usage{
stpp(x, y, t, stw)
}
\arguments{
  \item{x}{
A vector of x coordinates.
}
  \item{y}{
A vector of y coordinates.
}
  \item{t}{
A vector of t coordinates.
}
  \item{stw}{
An object of class \code{\dQuote{stwin}}.
}
}
\details{
x, y, and t represent the coordinates of all observed points in the space-time window described by \code{stw}. If any points fall outside of the window, a warning message is returned, and those points are removed from the point pattern. All inclusive points are then ordered in ascending order according to the t coordinates.

If no space-time window is specified, the default is a unit cube.
}
\value{
Outputs an object of class \code{"stpp"}, which is a list of
	\item{x }{ A vector of x coordinates.}
	\item{y }{ A vector of y coordinates.}
	\item{t }{ A vector of t coordinates.}
	\item{stw }{ An object of class \code{"stwin"}.}
}
\author{
Robert Clements
}
\seealso{
\code{\link{stwin}}
}
\examples{
#==> create a stpp object <===#
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