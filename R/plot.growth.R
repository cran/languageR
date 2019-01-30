`plot.growth` <-
function(x, w = "all", ...) {
  if (!is(x, "growth")) stop("argument should be a growth object")
  res = x@data$data
  if (w == "all") {
    graphics::par(mfrow = c(2, 4))
    graphics::plot(res$Tokens, res$Types, xlab = "tokens", ylab = "types")
    graphics::plot(res$Tokens, res$HapaxLegomena/res$Tokens, xlab = "tokens",
      ylab = "growth rate")
    graphics::plot(res$Tokens, res$TypeTokenRatio, xlab = "tokens", ylab = 
      "type-token ratio")
    graphics::plot(res$Tokens, res$Lognormal, xlab = "tokens", 
      ylab = "mean log frequency")
    graphics::plot(res$Tokens, res$Herdan, xlab = "tokens", ylab = "Herdan's C")
    graphics::plot(res$Tokens, res$Guiraud, xlab = "tokens", ylab = "Guiraud's R")
    graphics::plot(res$Tokens, res$Yule, xlab = "tokens", ylab = "Yule's K")
    graphics::plot(res$Tokens, res$Zipf, xlab = "tokens", ylab = "Zipf slope")
    graphics::par(mfrow = c(1, 1))
  } else {
    if (w %in% colnames(res)) {
      graphics::plot(res$Tokens, res[, w], xlab="Tokens", ylab=w)
    } else
      stop("w should specify a valid column name")
  }
}

