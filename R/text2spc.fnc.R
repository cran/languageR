`text2spc.fnc` <-
function(text) {
  if (!requireNamespace("zipfR", quietly = TRUE)) {
    stop("please install the zipfR library first")
  } else {
    tab = table(table(text))
    return(zipfR::spc(m = as.numeric(names(tab)), Vm = as.numeric(tab)))
  }
}

