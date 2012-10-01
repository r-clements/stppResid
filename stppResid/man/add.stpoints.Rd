\name{add.stpoints}
\alias{add.stpoints}
\title{Add space-time points to a stpp object}
\description{
This function adds new points to a \code{stpp} object.
}
\usage{
add.stpoints(X, points)
}
\arguments{
  \item{X}{
A space-time object of class \dQuote{\code{stpp}} to add points to.
}
  \item{points}{
A vector, data frame, matrix or list of points to be added to \code{X}. 
}
}
\details{
\code{points} can be a vector of length three describing one x, y, and t coordinate, or a matrix or data frame where the first column is a column of x coordinates, the second column is a column of y coordinates, and the third column is a column of t coordinates.  If \code{points} is a list, the first entry is a vector of x coordinates, the second entry is a vector of y coordinates, and the third entry is a vector of t coordinates.   
}
\value{
\code{add.stpoints} returns an object of class \dQuote{\code{stpp}}.
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

#===> add new points to X <===#
x.new <- rnorm(10, mean = 10, sd = 1)
y.new <- rnorm(10, mean = 100, sd = 10)
t.new <- runif(10, 0, 100)
all.new <- data.frame(cbind(x.new, y.new, t.new))
Y <- add.stpoints(X, all.new)
}