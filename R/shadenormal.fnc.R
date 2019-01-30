`shadenormal.fnc` <- 
function(qnts = c(0.025, 0.975)) {

    x = seq(-3, 3, 0.01)           # set up range of Z-values
    graphics::plot(x, stats::dnorm(x), type = "l")  # plot density
    graphics::abline(h = 0)

    # add shading to left rejection region
    x1 = seq(-3, stats::qnorm(qnts[1]), qnts[1]/5) # x-coordinates left -> right
    y1 = stats::dnorm(x1, 0, 1)                    # y-coordinates left -> right
    graphics::polygon(c(x1, rev(x1)),                 # x left -> right -> left
      c(rep(0, length(x1)), rev(y1)),       # y left -> right -> left
      col = "lightgrey")


    # add shading to right rejection region
    x1 = seq(stats::qnorm(qnts[2]), 3, qnts[1]/5)
    y1 = stats::dnorm(x1, 0, 1)
    graphics::polygon(c(x1, rev(x1)), c(rep(0, length(x1)), rev(y1)), col = "lightgrey")
}
