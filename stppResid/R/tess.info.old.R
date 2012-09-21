tess.info <- function(X, cifunction, theta = NULL, areas, tl)
{
	data <- data.frame(cbind(X$x, X$y, X$t))
	names(data) <- c("x", "y", "t")
	if(is.null(theta)) {
		lamb1 <- cifunction(X)
		fun2 <- function(z)
		{
			Y <- rbind(data, z)
			Y <- Y[which(Y$t <= z[3]),]
			Y <- stpp(Y$x, Y$y, Y$t, stwin(X$xcoord, X$ycoord, X$tcoord))
			tail(cifunction(Y),1)
		}
	}
	if(!is.null(theta)) {
		lamb1 <- cifunction(X, theta = theta)
		fun2 <- function(z)
		{
			Y <- rbind(data, z)
			Y <- Y[which(Y$t <= z[3]),]
			Y <- stpp(Y$x, Y$y, Y$t, stwin(X$xcoord, X$ycoord, X$tcoord))
			tail(cifunction(Y, theta),1)
		}
	}
	all <- function(tl)
	{
		cells <- as.points(tl$x, tl$y)
		place <- inpip(all.p[ ,1:2], cells)
		lamb.ave <- mean(lamb2[place])
		lamb.sd <- sd(lamb2[place])
		lamb.sd[which(is.na(lamb.sd))] <- lamb.ave[which(is.na(lamb.sd))]
		n2 <- length(place)
		return(c(lamb.ave, lamb.sd, n2))
	}
	vol <- areas * diff(X$tcoord)
	e <- 0
	n <- 0
	lamb2 <- lamb1
	all.p <- data
	while(e < length(length(X$x))) {
		n <- n + 10000
		cat("total number of points used to estimate integrals: ", n, "\n")
		xpoints <- runif(10000, X$xcoord[1], X$xcoord[2])
		ypoints	<- runif(10000, X$ycoord[1], X$ycoord[2])
		tpoints <- runif(10000, X$tcoord[1], X$tcoord[2])
		new.p <- data.frame(cbind(xpoints, ypoints, tpoints))
		names(new.p) <- c("x", "y", "t")
		lamb.t <- apply(new.p, 1, fun2)
		lamb2 <- c(lamb2, lamb.t)
		all.p <- rbind(all.p, new.p)
		info <- lapply(tl, all)
		lamb2.ave <- unlist(lapply(info, function(w){w[1]}))
		lamb2.sd <- unlist(lapply(info, function(w){w[2]}))
		n3 <- unlist(lapply(info, function(w){w[3]}))
		e <- sum(lamb2.sd/sqrt(n3) < lamb2.ave/100) 
	}
	int.approx <- lamb2.ave * vol	
	tess <- list(n = n, integral = int.approx, mean.lambda = lamb2.ave, sd.lambda = lamb2.sd)
	class(tess) <- "tess.info"
	return(tess)
}