print.summary.thinresid <- function(X)
{
	cat("Thinning rate: ", X$k, "\n")
	cat("Number of residuals: ", X$n, "\n")
	cat("Expected number of residuals: ", X$n.exp, "\n")
	cat("One-tailed p-value: ", X$p.val)
}