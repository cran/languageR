`pairscor.fnc` <-
function(data, hist = TRUE, smooth = TRUE,
  cex.points = 1,  col.points = "darkgrey") {
  panel.hist <- function(x, ...) {
    usr <- graphics::par("usr"); on.exit(graphics::par(usr))
    graphics::par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    graphics::rect(breaks[-nB], 0, breaks[-1], y, ...)
  }

  pairscor.lower <- function(x, y, ...) {
    usr <- graphics::par("usr"); on.exit(graphics::par(usr))
    graphics::par(usr = c(0, 1, 0, 1))
	  m = stats::cor.test(x, y)
	  r = round(m$estimate, 2)
	  p = round(m$p.value, 4)
	  rtxt = paste("r =", r)
	  ptxt = paste("p =", p)

	  options(warn=-1)  # ignore warnings
	  m2 = stats::cor.test(x, y, method="spearman")
	  r2 = round(m2$estimate, 2)
	  p2 = round(m2$p.value, 4)
	  rtxt2 = paste("rs =", r2)
	  ptxt2 = paste("p =", p2)
	  options(warn=0)

	  graphics::text(0.5, 0.8, rtxt)
	  graphics::text(0.5, 0.6, ptxt)
	  graphics::lines(c(0.2,0.8),c(0.5,0.5))
	  graphics::text(0.5, 0.4, rtxt2)
	  graphics::text(0.5, 0.2, ptxt2)
  }
  panel.smooth2 = function (x, y, col = graphics::par("col"), bg = NA, pch = graphics::par("pch"),
    cex = 1, span = 2/3, iter = 3, ...) {
    graphics::points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        graphics::lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = "black", ...)
  }


  if (hist == TRUE) {
    if (smooth == TRUE) {
	     graphics::pairs(data, 
         diag.panel = panel.hist,
         lower.panel = pairscor.lower, 
         upper.panel = panel.smooth2, col = col.points,
           cex = cex.points)
    } else {
       graphics::pairs(data, 
         diag.panel = panel.hist,
         lower.panel = pairscor.lower) 
    }
  } else {
    if (smooth == TRUE) {
	    graphics::pairs(data, lower.panel = pairscor.lower, 
        upper.panel = panel.smooth2, col = col.points,
        cex = cex.points)
    } else {
	    graphics::pairs(data, lower.panel = pairscor.lower) 
    }
  }
}

