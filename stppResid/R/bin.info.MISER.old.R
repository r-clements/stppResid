bin.info.MISER <- function(X, cifunction, theta = NULL, gf, type = c(1,2), n)
{
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
	int.fun <- function(g)
	{
		bin.pts <- app.pts[which((app.pts$x >= g[1]) & (app.pts$x < g[2]) & (app.pts$y >= g[3]) & (app.pts$y < g[4])) , 4]
		if(type == 1)
			int.bin <- mean(bin.pts)*vol
		if(type == 2)
			int.bin <- mean(sqrt(bin.pts))*vol
			
		c(int.bin, mean(bin.pts), sd(bin.pts))
	}
	vol <- diff(X$xcoord) * diff(X$ycoord) * diff(X$tcoord) / nrow(gf$grid.full)
	data <- data.frame(cbind(X$x, X$y, X$t))
	names(data) <- c("x", "y", "t")
	xrange <- X$xcoord
	yrange <- X$ycoord
	trange <- X$tcoord
	n.start <- n*.1
	x1 <- runif(n.start, xrange[1], xrange[2])
	y1 <- runif(n.start, yrange[1], yrange[2])
	t1 <- runif(n.start, trange[1], trange[2])
	N <- n - n.start
	miser <- function(xrange, yrange, trange, x1, y1, t1, N)
	{
		all.pts <- data.frame(cbind(x1, y1, t1))
		names(all.pts) <- c("x", "y", "t")
		all.l <- apply(all.pts, 1, fun2)
		lX1 <- all.l[which((all.pts[,1] > xrange[1]) & (all.pts[,1] <= median(xrange)))]
		lX2 <- all.l[which((all.pts[,1] > median(xrange)) & (all.pts[,1] < xrange[2]))]
		lY1 <- all.l[which((all.pts[,2] > yrange[1]) & (all.pts[,2] <= median(yrange)))]
		lY2 <- all.l[which((all.pts[,2] > median(yrange)) & (all.pts[,2] < yrange[2]))]
		lT1 <- all.l[which((all.pts[,3] > trange[1]) & (all.pts[,3] <= median(trange)))]
		lT2 <- all.l[which((all.pts[,3] > median(trange)) & (all.pts[,3] < trange[2]))]
		if((length(lX1) > 1) & (length(lX2) > 1)) {
			sX1 <- sd(lX1)
			sX2 <- sd(lX2)
			sXsum <- sX1 + sX2
		} else sXsum <- Inf
		if((length(lY1) > 1) & (length(lY2) > 1)) {
			sY1 <- sd(lY1)
			sY2 <- sd(lY2)
			sYsum <- sY1 + sY2
		} else sYsum <- Inf
		if((length(lT1) > 1) & (length(lT2) > 1)) {
			sT1 <- sd(lT1)
			sT2 <- sd(lT2)
			sTsum <- sT1 + sT2
		} else sTsum <- Inf
		S <- c(sXsum, sYsum, sTsum)
		all.pts <- cbind(all.pts, all.l)
		names(all.pts) <- c("x", "y", "t", "l")
		if(((S[1] == Inf) & (S[2] == Inf) & (S[3] == Inf))) {
			x1.1 <- runif(N, xrange[1], xrange[2])
			y1.1 <- runif(N, yrange[1], yrange[2])
			t1.1 <- runif(N, trange[1], trange[2])
			all.pts.1 <- data.frame(cbind(x1.1, y1.1, t1.1))
			names(all.pts.1) <- c("x", "y", "t")
			l <- apply(all.pts.1, 1, fun2)
			all.pts.1 <- cbind(all.pts.1, l)
			all.pts <- rbind(all.pts, all.pts.1)
			return(all.pts)
			}
		if (((S[1] != Inf) | (S[2] != Inf) | (S[3] != Inf))) {
			strat <- which(S==min(S))
		if(length(strat) > 1)
			strat <- strat[1]
			if(strat == 1) {
				N.a <- ceiling((sX1)/(sX1+sX2)*N)
				if(N.a > 10) {
					n.a <- ceiling(.1*N.a)
					n.a2 <- N.a-n.a
					xa <- runif(n.a, xrange[1], median(xrange))
					ya <- runif(n.a, yrange[1], yrange[2])
					ta <- runif(n.a, trange[1], trange[2])
					xrange.a <- c(xrange[1], median(xrange))
					ALL.pts <- miser(xrange.a, yrange, trange, xa, ya, ta, n.a2)
				}
				if((N.a <= 10) & (N.a > 0)) {
					xa <- runif(N.a, xrange[1], median(xrange))
					ya <- runif(N.a, yrange[1], yrange[2])
					ta <- runif(N.a, trange[1], trange[2])
					ALL.pts <- data.frame(cbind(xa, ya, ta))
					names(ALL.pts) <- c("x", "y", "t")
					l <- apply(ALL.pts, 1, fun2) 
					ALL.pts <- cbind(ALL.pts, l) 
				}
				if(N.a == 0) {
					ALL.pts <- data.frame()	
				}
				N.b <- N-N.a
				if(N.b > 10) {
					n.b <- ceiling(.1*N.b)
					n.b2 <- N.b-n.b
					xb <- runif(n.b, median(xrange), xrange[2])
					yb <- runif(n.b, yrange[1], yrange[2])
					tb <- runif(n.b, trange[1], trange[2])
					xrange.b <- c(median(xrange), xrange[2])
					ALL.pts2 <- miser(xrange.b, yrange, trange, xb, yb, tb, n.b2)
				}
				if((N.b <= 10) & (N.b > 0)) {
					xb <- runif(N.b, median(xrange), xrange[2])
					yb <- runif(N.b, yrange[1], yrange[2])
					tb <- runif(N.b, trange[1], trange[2])
					ALL.pts2 <- data.frame(cbind(xb, yb, tb))
					names(ALL.pts2) <- c("x", "y", "t")
					l <- apply(ALL.pts2, 1, fun2) 
					ALL.pts2 <- cbind(ALL.pts2, l) 
				}
				if(N.b == 0) {
					ALL.pts2 <- data.frame()
				}
				total.pts <- rbind(all.pts, ALL.pts, ALL.pts2)
				return(total.pts)
			}
			if(strat == 2){
				N.a <- ceiling((sY1)/(sY1+sY2)*N)
				if(N.a > 10) {
					n.a <- ceiling(.1*N.a)
					n.a2 <- N.a-n.a
					xa <- runif(n.a, xrange[1], xrange[2])
					ya <- runif(n.a, yrange[1], median(yrange))
					ta <- runif(n.a, trange[1], trange[2])
					yrange.a <- c(yrange[1], median(yrange))
					ALL.pts <- miser(xrange, yrange.a, trange, xa, ya, ta, n.a2)
				}
				if((N.a <= 10) & (N.a > 0)) {
					xa <- runif(N.a, xrange[1], xrange[2])
					ya <- runif(N.a, yrange[1], median(yrange))
					ta <- runif(N.a, trange[1], trange[2])
					ALL.pts <- data.frame(cbind(xa, ya, ta))
					names(ALL.pts) <- c("x", "y", "t")
					l <- apply(ALL.pts, 1, fun2) 
					ALL.pts <- cbind(ALL.pts, l) 
				}
				if(N.a == 0) {
					ALL.pts <- data.frame()	
				}
			
				N.b <- N-N.a
				if(N.b > 10) {
					n.b <- ceiling(.1*N.b)
					n.b2 <- N.b-n.b
					xb <- runif(n.b, xrange[1], xrange[2])
					yb <- runif(n.b, median(yrange), yrange[2])
					tb <- runif(n.b, trange[1], trange[2])
					yrange.b <- c(median(yrange), yrange[2])
					ALL.pts2 <- miser(xrange, yrange.b, trange, xb, yb, tb, n.b2)
				}
				if((N.b <= 10) & (N.b > 0)) {
					xb <- runif(N.b, xrange[1], xrange[2])
					yb <- runif(N.b, median(yrange), yrange[2])
					tb <- runif(N.b, trange[1], trange[2])
					ALL.pts2 <- data.frame(cbind(xb, yb, tb))
					names(ALL.pts2) <- c("x", "y", "t")
					l <- apply(ALL.pts2, 1, fun2) 
					ALL.pts2 <- cbind(ALL.pts2, l)
				}
				if(N.b == 0) {
					ALL.pts2 <- data.frame()
				}
				total.pts <- rbind(all.pts, ALL.pts, ALL.pts2)
				return(total.pts)
			}
			if(strat == 3){
				N.a <- ceiling((sT1)/(sT1+sT2)*N)
				if(N.a > 10) {
					n.a <- ceiling(.1*N.a)
					n.a2 <- N.a-n.a
					xa <- runif(n.a, xrange[1], xrange[2])
					ya <- runif(n.a, yrange[1], yrange[2])
					ta <- runif(n.a, trange[1], median(trange))
					trange.a <- c(trange[1], median(trange))
					ALL.pts <- miser(xrange, yrange, trange.a, xa, ya, ta, n.a2)
				}
				if((N.a <= 10) & (N.a > 0)) {
					xa <- runif(N.a, xrange[1], xrange[2])
					ya <- runif(N.a, yrange[1], yrange[2])
					ta <- runif(N.a, trange[1], median(trange))
					ALL.pts <- data.frame(cbind(xa, ya, ta))
					names(ALL.pts) <- c("x", "y", "t")
					l <- apply(ALL.pts, 1, fun2) 
					ALL.pts <- cbind(ALL.pts, l) 
				}
				if(N.a == 0) {
					ALL.pts <- data.frame()	
				}
			
				N.b <- N-N.a
				if(N.b > 10) {
					n.b <- ceiling(.1*N.b)
					n.b2 <- N.b-n.b
					xb <- runif(n.b, xrange[1], xrange[2])
					yb <- runif(n.b, yrange[1], yrange[2])
					tb <- runif(n.b, median(trange), trange[2])
					trange.b <- c(median(trange), trange[2])
					ALL.pts2 <- miser(xrange, yrange, trange.b, xb, yb, tb, n.b2)
				}
				if((N.b <= 10) & (N.b > 0)) {
					xb <- runif(N.b, xrange[1], xrange[2])
					yb <- runif(N.b, yrange[1], yrange[2])
					tb <- runif(N.b, median(trange), trange[2])
					ALL.pts2 <- data.frame(cbind(xb, yb, tb))
					names(ALL.pts2) <- c("x", "y", "t")
					l <- apply(ALL.pts2, 1, fun2) 
					ALL.pts2 <- cbind(ALL.pts2, l) 
				}
				if(N.b == 0) {
					ALL.pts2 <- data.frame()
				}
				total.pts <- rbind(all.pts, ALL.pts, ALL.pts2)
				return(total.pts)
			}
		}
	}
	app.pts <- miser(xrange, yrange, trange, x1, y1, t1, N)
	all.df <- data.frame(t(apply(gf$grid.full, 1, int.fun)))
	bins <- list(n=n, integral = all.df[,1], mean.lambda = all.df[,2], sd.lambda = all.df[,3], app.pts = app.pts)
	class(bins) <- "bin.info"
	return(bins)
}
