plot.tessresid <- function(x, ..., col.key = rev(heat.colors(100)), cutoffs = NULL, plot.smooth = FALSE, smooth.col = heat.colors, nlevels = 20)
{
  X <- x
	if(plot.smooth == FALSE) {
		residuals <- X$residuals
		if(is.null(cutoffs))	{
			cutoffs <- seq(min(residuals) - 1e-09, max(residuals) + 1e-09, length.out = length(col.key)+1)
			cutoff.m <- data.frame(cbind(cutoffs[-length(cutoffs)], cutoffs[-1]))
			CL.res <- rep(0, length(residuals))
			for(j in 1:length(residuals)) {
				CL.res[j] <- col.key[which((cutoff.m[,2] >= residuals[j]) & (cutoff.m[,1] < residuals[j]))]	
			}
		}
		else {
			if((length(cutoffs) - 1) != length(col.key))
				stop("length of cutoffs should be 1 more than length of col.key")
			if(!is.character(col.key))
				stop("col.key must be character vector of colors in hexadecimal")
			cutoff.m <- data.frame(cbind(cutoffs[-length(cutoffs)], cutoffs[-1]))
			CL.res <- rep(0, length(residuals))
			for(j in 1:length(residuals)) {
				CL.res[j] <- col.key[which((cutoff.m[,2] >= residuals[j]) & (cutoff.m[,1] < residuals[j]))]	
			}
		}
		dev.new()
		layout(matrix(c(1,2), ncol=1), heights=c(2,.5))
		par(mar=c(4,4,4,4), bty="n")
		plot(X[[1]]$x, X[[1]]$y, type = "n", xlab = "x", ylab = "y", xlim = X[[1]]$xcoord, ylim = X[[1]]$ycoord)
		for(i in 1:length(X[[2]])) {
			temp <- X[[2]][i]
			x <- temp[[1]]$x
			y <- temp[[1]]$y
			p <- polygon(x, y, col = CL.res[i])	
		}
		points(X[[1]]$x, X[[1]]$y, ...)
		par(mar=c(1, 1, 1.5, 1))
		key <- (0:length(CL.res))/length(CL.res)
		plot(NULL, ylim=c(-3,0), xlim=c(-0.2, 1.2), type="n", axes=F, xlab="", ylab="", main="")
		image(key, -2:0, matrix(rep(key,2), nrow=length(CL.res)+1, byrow=F), add=T, col=col.key)
		for(i in seq(0, 1, length=10)) lines(c(i,i), c(-2,0), col=gray(0.3), lty=2)
		text(0, -2.5, round(min(cutoffs), 3), cex=1)
		text(0.25, -2.5, round(quantile(cutoffs, 0.25), 3), cex=1)
		text(0.5, -2.5, round(median(cutoffs), 3), cex=1)
		text(0.75, -2.5, round(quantile(cutoffs, 0.75), 3), cex=1)
		text(1, -2.5, round(max(cutoffs), 3), cex=1)
		mtext("Voronoi tessellation residuals", 3)
	} else {
		tl <- X$tile.list
		resids <- X$residuals
		xave <- seq(X[[1]]$xcoord[1], X[[1]]$xcoord[2], length.out=102)
		xave <- xave[-c(1,102)]
		yave <- seq(X[[1]]$ycoord[1], X[[1]]$ycoord[2], length.out=102)
		yave <- yave[-c(1,102)]
		plotgrid <- expand.grid(x=xave, y=yave)
		gridpts <- as.points(plotgrid$x, plotgrid$y)
		placefctn <- function(tl) {
			cell <- as.points(tl$x, tl$y)
			place <- inpip(gridpts, cell)
			return(c(place))
		}
		places <- lapply(tl, placefctn)
		lambs <- rep(0, 100*100)
		for(i in 1:length(places)) {
			here <- places[[i]] 
			lambs[here] <- resids[i]
		}
		zlambs <- matrix(lambs, nrow = length(xave), byrow = TRUE)
		dev.new()
		filled.contour(xave, yave, zlambs, xlab = "x", ylab = "y", nlevels = nlevels, color.palette = smooth.col, plot.axes = {axis(1); axis(2); points(X[[1]]$x, X[[1]]$y, ...)})
	}
}