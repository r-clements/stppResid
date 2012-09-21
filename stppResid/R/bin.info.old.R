bin.info <- function(X, cifunction, theta = NULL, gf, type = c(1,2), n.def)
{
	xpoints <- function(x) 
	{
		y <- runif(n.def, x[1], x[2])
		return(y)	
	}
	ypoints <- function(x)
	{
		y <- runif(n.def, x[3], x[4])	
		return(y)
	}
	if(is.null(theta)) {
		fun2 <- function(z)
		{
			Y <- rbind(data, z)
			Y <- Y[which(Y$t <= z[3]),]
			Y <- stpp(Y$x, Y$y, Y$t, stwin(X$xcoord, X$ycoord, X$tcoord))
			tail(cifunction(Y),1)
		}
	}
	if(!is.null(theta)) {
		fun2 <- function(z)
		{
			Y <- rbind(data, z)
			Y <- Y[which(Y$t <= z[3]),]
			Y <- stpp(Y$x, Y$y, Y$t, stwin(X$xcoord, X$ycoord, X$tcoord))
			tail(cifunction(Y, theta),1)
		}
	}
	all <- function(x)
	{
		div <- length(x)/3
		xt <- x[1:div]
		yt <- x[(div+1):(2*div)]
		tt <- x[(2*div+1):(3*div)]
		new.pts <- data.frame(cbind(xt, yt, tt))
		new.pts <- new.pts[order(new.pts[,3]),]
		lamb2 <- apply(new.pts, 1, fun2)
		return(lamb2)
	}
	vol <- diff(X$xcoord) * diff(X$ycoord) * diff(X$tcoord) / nrow(gf$grid.full)
	data <- data.frame(cbind(X$x, X$y, X$t))
	names(data) <- c("x", "y", "t")
	e <- 0
	n <- 0
	lamb2 <- c()
	while(e < nrow(gf$grid.full)) {
		n = n + n.def
		cat("number of points per bin: ", n, "\n")
		xp <- apply(gf$grid.full, 1, xpoints)
		yp <- apply(gf$grid.full, 1, ypoints)
		tp.t <- runif(n.def*nrow(gf$grid.full), X$tcoord[1], X$tcoord[2])
		tp <- matrix(tp.t, ncol = nrow(gf$grid.full))
		new.p <- rbind(xp, yp, tp)
		lamb2 <- rbind(lamb2, apply(new.p, 2, all))
		if(type == 1) {
			lamb2.ave <- apply(lamb2, 2, mean)
			lamb2.sd <- apply(lamb2, 2, sd)
		}
		else {
			lamb2.ave <- apply(sqrt(lamb2), 2, mean)
			lamb2.sd <- apply(sqrt(lamb2), 2, sd)
		}
		e <- sum(lamb2.sd/sqrt(n) < lamb2.ave/100)
	}
	int.approx <- lamb2.ave * vol
	bins <- list(n = n, integral = int.approx, mean.lambda = lamb2.ave, sd.lambda = lamb2.sd)
	class(bins) <- "bin.info"
	return(bins)
}
