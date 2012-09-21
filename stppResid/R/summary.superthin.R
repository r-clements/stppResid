summary.superthin <- function(X)
{
	k <- X$k
	n <- nrow(X$residuals)
	vol <- diff(X[[1]]$xcoord) * diff(X[[1]]$ycoord) * diff(X[[1]]$tcoord)
	n.exp <- k * vol
	if(n < n.exp)
		p.val <- ppois(n, n.exp) 
	if(n > n.exp)
		p.val <- ppois(n, n.exp, lower.tail = FALSE) 
	if(n == n.exp)
		p.val <- 1
	Y <- list(k = k, n = n, n.exp = n.exp, p.val = p.val)
	class(Y) <- "summary.superthin"
	return(Y)
}