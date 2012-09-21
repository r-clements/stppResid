plot.supresid <- function(X, pch1 = 1, pch2 = 3, asp = 1, ...)
{
	if(!is.supresid(X))
		stop("X must be an object of type supresid")
	plot(X[[1]]$x, X[[1]]$y, xlim = X[[1]]$xcoord, ylim = X[[1]]$ycoord, pch = pch1, asp = asp, xlab = "x", ylab = "y", ...)
	points(X[[4]]$x, X[[4]]$y, pch = pch2)
}