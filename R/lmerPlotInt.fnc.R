lmerPlotInt.fnc = function(lmermodel, xname, yname, intxyname, 
qntls=seq(0,1,by=0.1), view = 30, addStdError = FALSE, ndigits=2,
  nlev=30, which="matplot", shadow = 0.5, colour = "lightblue", fun=NA, ylabel=NA, ...){

  requireNamespace("lme4", quietly = TRUE)

  if (!(lme4::isLMM(lmermodel) | lme4::isGLMM(lmermodel)))
    stop("model object must be fitted with lmer or glmer")
  f <- function(x,y) {return(intercept + slopeX*x + slopeY*y + interactionxy*x*y)}

  if (!is.function(fun)) {
     if (!is.na(fun)) {
       stop("fun should be a function (not the name of a function)\n")
     } else {
       fun = function(v) return (v)
     }
  } 
 


  coefs = lme4::fixef(lmermodel)
  intercept = coefs["(Intercept)"]

  if (xname %in% names(coefs)) slopeX = coefs[xname]
  else stop(paste(xname, "is not a valid predictor name"))

  if (yname %in% names(coefs)) slopeY = coefs[yname]
  else stop(paste(yname, "is not a valid predictor name"))

  if (intxyname %in% names(coefs)) interactionxy = coefs[intxyname]
  else stop(paste(intxyname, "is not a valid predictor name"))

  dat = lmermodel@frame

   
  x = unique(as.numeric(stats::quantile(dat[,xname], qntls)))
  y = unique(as.numeric(stats::quantile(dat[,yname], qntls)))
  z <- outer(x, y, f)

  if (addStdError) {
    sdError = lmermodel@devComp["scale"]
    z1 = matrix(stats::rnorm(length(z), 0, sdError), nrow(z), ncol(z))
    z = z + z1
  }

  pars = graphics::par(c("mfrow", "mar"))
  if (which == "all") {
     graphics::par(mfrow=c(2,2), mar=c(4.5,4,1,1))
  }

  if ((which == "persp") | (which == "all")) {
    graphics::persp(x, y, z, theta=view, col=colour, ticktype = "detailed", 
           xlab=xname, ylab=yname,zlab=as.character(stats::formula(lmermodel))[2],
           shade=shadow, phi=20)
  }     
  if ((which == "contour") | (which == "all")) {
    graphics::contour(x, y, z, col="blue", nlevel=nlev, xlab=xname, ylab=yname)
  } 
  if ((which == "matplot") | (which == "all")) {
    offset = (max(x)-min(x))/5
    if (is.function(fun))  z = fun(z)
      ylbl = as.character(stats::formula(lmermodel))[2]
      if (!is.na(ylabel)) ylbl = ylabel
    graphics::matplot(x, z, xlab=xname,type="l",ylab=ylbl,
       xlim=c(min(x), max(x)+offset),...)
    x1 = rep(x[length(x)],length(y))
    y1 = f(x1, y)
    if (is.function(fun)) y1 = fun(y1)
    if (which == "all") graphics::text(x1+offset, y1, round(y,ndigits), adj=+0.8, cex=0.8) 
    else graphics::text(x1+offset, y1, round(y,ndigits), adj=+0.8, cex=0.7)
    graphics::mtext(yname,4, line=1,adj=0,cex=0.8)
  }
  if ((which == "image") | (which == "all")) {
    graphics::image(x, y, z, col = grDevices::heat.colors(10))
  }
  if (which == "all") graphics::par(pars)
}

