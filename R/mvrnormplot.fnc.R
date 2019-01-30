`mvrnormplot.fnc` <- 
function(r = 0.9, n = 100, limits=NA) {
	requireNamespace("MASS", quietly = TRUE)
  x = MASS::mvrnorm(n, mu = c(0, 0), Sigma=cbind(c(1, r), c(r, 1)))
	if (is.na(limits)) 
    graphics::plot(x, xlab = "x", ylab = "y")
	else
    graphics::plot(x, xlab = "x", ylab = "y", ylim=limits, xlim=limits)
  graphics::abline(stats::lm(x[, 2] ~ x[, 1]))
  graphics::mtext(paste("r =", round(stats::cor(x)[1, 2], 3)), line = 1)
	if (r > 0)
    graphics::abline(0, 1, lty = 2)
  else
    graphics::abline(0, -1, lty = 2)
}
