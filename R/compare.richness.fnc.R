`compare.richness.fnc` <-
function(text1, text2, digits = 5) {

  if (!requireNamespace("zipfR", quietly = TRUE)) {
    stop("please install the zipfR library first")
  } else {

    text1.spc = text2spc.fnc(text1)
    N1 = zipfR::N(text1.spc)
    text2.spc = text2spc.fnc(text2)
    N2 = zipfR::N(text2.spc)

    text1.gigp = zipfR::lnre("gigp", text1.spc)
    text1.fzm = zipfR::lnre("fzm", text1.spc)
    if (text1.gigp$gof$X2 <= text1.fzm$gof$X2) {
       model1 = "gigp"
       Text1VarV = zipfR::VV(text1.gigp, N1)
       Text1VarV1 = zipfR::VVm(text1.gigp, 1, N1)
       Text1X2 = text1.gigp$gof$X2
    } else {
       model1 = "fzm"
       Text1VarV = zipfR::VV(text1.fzm, N1)
       Text1VarV1 = zipfR::VVm(text1.fzm, 1, N1)
       Text1X2 = text1.fzm$gof$X2
    }

    text2.gigp = zipfR::lnre("gigp", text2.spc)
    text2.fzm = zipfR::lnre("fzm", text2.spc)
    if (text2.gigp$gof$X2 <= text2.fzm$gof$X2) {
       model2 = "gigp"
       Text2VarV = zipfR::VV(text2.gigp, N2)
       Text2VarV1 = zipfR::VVm(text2.gigp, 1, N2)
       Text2X2 = text2.gigp$gof$X2
    } else {
       model2 = "fzm"
       Text2VarV = zipfR::VV(text2.fzm, N1)
       Text2VarV1 = zipfR::VVm(text2.fzm, 1, N1)
       Text2X2 = text2.fzm$gof$X2
    }



    ZV = (zipfR::V(text1.spc) - zipfR::V(text2.spc))/sqrt(Text1VarV + Text2VarV)
    ZP = ((zipfR::Vm(text1.spc,1)/N1) - (zipfR::Vm(text2.spc, 1)/N2))/
         sqrt( ((1/(N1*N1)) * Text1VarV1) + ((1/(N2*N2)) * Text2VarV1) )

    x1 = c(N1, zipfR::V(text1.spc), zipfR::Vm(text1.spc, 1), zipfR::Vm(text1.spc, 1)/N1)
    x2 = c(N2, zipfR::V(text2.spc), zipfR::Vm(text2.spc, 1), zipfR::Vm(text2.spc, 1)/N2)
    res = data.frame(rbind(x1, x2))
    argNames = as.character(sys.call())[2:3]
    rownames(res) = argNames
    colnames(res) = c("Tokens", "Types", "HapaxLegomena", "GrowthRate")
    res$GrowthRate = round(res$GrowthRate, digits)
    cat("\ncomparison of lexical richness for", argNames[1], "and",
     argNames[2], "\n")
    cat("with approximations of variances based on the LNRE models\n")
    cat(paste(model1, " (X2 = ", round(Text1X2, 2), ") and ", 
        model2, " (X2 = ", round(Text2X2, 2), ")\n\n", sep=""))
    print(res)
    cat("\ntwo-tailed tests:\n\n")
    tab = data.frame(Z = c(round(ZV, 4), round(ZP, 4)), 
          p = c(round(2*(1-stats::pnorm(abs(ZV))),4), round(2*(1-stats::pnorm(abs(ZP))), 4)))
    rownames(tab) = c("Vocabulary Size", "Vocabulary Growth Rate")
    print(tab)
  }
}
