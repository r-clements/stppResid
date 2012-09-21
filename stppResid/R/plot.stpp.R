plot.stpp <- function(X, pch = 1, asp = 1, ...)
{
	if(!is.stpp(X))
		stop("X must be an object of type stpp")
	plot(X$x, X$y, xlim = X$xcoord, ylim = X$ycoord, pch = pch, asp = asp, xlab = "x", ylab = "y", ...)
}