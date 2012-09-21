plot.superthin <- function(X, pch1 = 1, pch2 = 3, asp = 1, ...)
{
	if(!is.superthin(X))
		stop("X must be an object of type superthin")
	plot(X[[5]]$x, X[[5]]$y, xlim = X[[1]]$xcoord, ylim = X[[1]]$ycoord, pch = pch1, asp = asp, xlab = "x", ylab = "y", ...)
	points(X[[6]]$x, X[[6]]$y, pch = pch1)
	points(X[[4]]$x, X[[4]]$y, pch = pch2)	
}