plot.thinresid <- function(X, pch = 1, asp = 1, ...)
{
	if(!is.thinresid(X))
		stop("X must be an object of type thinresid")
	plot(X[[3]]$x, X[[3]]$y, xlim = X[[1]]$xcoord, ylim = X[[1]]$ycoord, pch = pch, asp = asp, xlab = "x", ylab = "y", ...)
}