exchangeDij <- function(ctmatrix){
  exchDij <- 2 * pmin(ctmatrix, t(ctmatrix))
  exchDij.lt <- lower.tri(exchDij) * exchDij
  return(exchDij.lt)
}

exchangeDj <- function(ctmatrix){
  exchDij <- 2 * pmin(ctmatrix, t(ctmatrix))
  exchDij.lt <- lower.tri(exchDij) * exchDij
  exchDj <- apply(exchDij.lt, 1, sum) + apply(exchDij.lt, 2, sum)
  return(exchDj)
}

overallAllocD <- function(ctmatrix){
  overallallocd <- overallDiff(ctmatrix) - overallQtyD(ctmatrix)
  return(overallallocd)
}

overallDiff <- function(ctmatrix){
  overalldiff <- sum(ctmatrix) - sum(diag(ctmatrix))
  return(overalldiff)
}

overallDiffCatj <- function(ctmatrix){
  overalldiffj <- apply(ctmatrix, 1, sum) + apply(ctmatrix, 2, sum) - 2 * diag(ctmatrix)
  return(overalldiffj)
}

overallExchangeD <- function(ctmatrix){
  Exchange <- exchangeDj(ctmatrix)
  overallexcd <- sum(Exchange) / 2
  return(overallexcd)
}

overallQtyD <- function(ctmatrix){
  qtydj <- abs(apply(ctmatrix, 1, sum) - apply(ctmatrix, 2, sum))
  overallqtyd <- sum(qtydj) / 2
  return(overallqtyd)
}

overallShiftD <- function(ctmatrix){
  Shift <- shiftDj(ctmatrix)
  overallshfd <- sum(Shift) / 2
  return(overallshfd)
}

quantityDj <- function(ctmatrix){
  qtydj <- abs(apply(ctmatrix, 1, sum) - apply(ctmatrix, 2, sum))
  return(qtydj)
}

shiftDj <- function(ctmatrix){
  shiftdj <- overallDiffCatj(ctmatrix) - quantityDj(ctmatrix) - exchangeDj(ctmatrix)
  return(shiftdj)
}
