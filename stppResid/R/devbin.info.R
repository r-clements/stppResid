devbin.info <- function(X, cifunction1, cifunction2, theta1 = NULL, theta2 = NULL, gf)
{
	xpoints <- function(x)
	{
		y <- runif(100, x[1], x[2])
		return(y)	
	}
	ypoints <- function(x)
	{
		y <- runif(100, x[3], x[4])	
		return(y)
	}
	tpoints <- function(x)
	{
		y <- runif(100, x$tcoord[1], x$tcoord[2])	
		return(y)
	}
	if(is.null(theta1)) {
		fun2.1 <- function(z)
		{
			Y <- rbind(data, z)
			Y <- Y[which(Y$t <= z[3]),]
			Y <- stpp(Y$x, Y$y, Y$t, stwin(X$xcoord, X$ycoord, X$tcoord))
			tail(cifunction1(Y),1)
		}
		fun2.2 <- function(z)
		{
			Y <- rbind(data, z)
			Y <- Y[which(Y$t <= z[3]),]
			Y <- stpp(Y$x, Y$y, Y$t, stwin(X$xcoord, X$ycoord, X$tcoord))
			tail(cifunction2(Y),1)
		}
	}
	if(!is.null(theta1)) {
		fun2.1 <- function(z)
		{
			Y <- rbind(data, z)
			Y <- Y[which(Y$t <= z[3]),]
			Y <- stpp(Y$x, Y$y, Y$t, stwin(X$xcoord, X$ycoord, X$tcoord))
			tail(cifunction1(Y, theta1),1)
		}
		fun2.2 <- function(z)
		{
			Y <- rbind(data, z)
			Y <- Y[which(Y$t <= z[3]),]
			Y <- stpp(Y$x, Y$y, Y$t, stwin(X$xcoord, X$ycoord, X$tcoord))
			tail(cifunction2(Y, theta2),1)
		}
	}
	all1 <- function(x)
	{
		div <- length(x)/3
		xt <- x[1:div]
		yt <- x[(div+1):(2*div)]
		tt <- x[(2*div+1):(3*div)]
		new.pts <- data.frame(cbind(xt, yt, tt))
		new.pts <- new.pts[order(new.pts[,3]),]
		lamb2.1 <- apply(new.pts, 1, fun2.1)
		return(lamb2.1)
	}
	all2 <- function(x)
	{
		div <- length(x)/3
		xt <- x[1:div]
		yt <- x[(div+1):(2*div)]
		tt <- x[(2*div+1):(3*div)]
		new.pts <- data.frame(cbind(xt, yt, tt))
		new.pts <- new.pts[order(new.pts[,3]),]
		lamb2.2 <- apply(new.pts, 1, fun2.2)
		return(lamb2.2)
	}
	vol <- diff(X$xcoord) * diff(X$ycoord) * diff(X$tcoord) / nrow(gf$grid.full)
	data <- data.frame(cbind(X$x, X$y, X$t))
	names(data) <- c("x", "y", "t")
	e1 <- 0
	e2 <- 0
	n1 <- 0
	n2 <- 0
	lamb2.1 <- c()
	lamb2.2 <- c()
	while(e1 < 1 || e2 < 1) {
		xp <- apply(gf$grid.full, 1, xpoints)
		yp <- apply(gf$grid.full, 1, ypoints)
		tp.t <- rep(tpoints(X), nrow(gf$grid.full))
		tp <- matrix(tp.t, ncol = nrow(gf$grid.full))
		new.p <- rbind(xp, yp, tp)
		if(e1 < nrow(gf$grid.full)) {
			n1 <- n1 + 100
			cat("model 1: number of points per bin: ", n1, "\n")
			lamb2.1 <- rbind(lamb2.1, apply(new.p, 2, all1))
			lamb2.1.ave <- apply(lamb2.1, 2, mean)
			lamb2.1.sd <- apply(lamb2.1, 2, sd)
			int1.approx <- lamb2.1.ave * vol
			e1 <- sum(lamb2.1.sd/sqrt(n1) < lamb2.1.ave/100)
		}
		if(e2 < nrow(gf$grid.full)) {
			n2 <- n2 + 100
			cat("model 2: number of points per bin: ", n2, "\n")
			lamb2.2 <- rbind(lamb2.2, apply(new.p, 2, all2))
			lamb2.2.ave <- apply(lamb2.2, 2, mean)
			lamb2.2.sd <- apply(lamb2.2, 2, sd)
			int2.approx <- lamb2.2.ave * vol
			e2 <- sum(lamb2.2.sd/sqrt(n2) < lamb2.2.ave/100)
		}
	}
	bins <- list(n1 = n1, n2 = n2, integral1 = int1.approx, integral2 = int2.approx, mean.lambda1 = lamb2.1.ave, mean.lambda2 = lamb2.2.ave, sd.lambda1 = lamb2.1.sd, sd.lambda2 = lamb2.2.sd)
	class(bins) <- "devbin.info"
	return(bins)
}