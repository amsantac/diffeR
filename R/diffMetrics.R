exchangeDij <- function(ctmatrix){
  exchDij <- 2 * pmin(ctmatrix, t(ctmatrix))
  res <- lower.tri(exchDij) * exchDij
  return(res)
}

exchangeDj <- function(ctmatrix){
  exchDij <- 2 * pmin(ctmatrix, t(ctmatrix))
  exchDij.lt <- lower.tri(exchDij) * exchDij
  res <- apply(exchDij.lt, 1, sum) + apply(exchDij.lt, 2, sum)
  return(res)
}

overallAllocD <- function(ctmatrix){
  res <- overallDiff(ctmatrix) - overallQtyD(ctmatrix)
  return(res)
}

overallDiff <- function(ctmatrix){
  res <- sum(ctmatrix) - sum(diag(ctmatrix))
  return(res)
}

overallDiffCatj <- function(ctmatrix){
  res <- apply(ctmatrix, 1, sum) + apply(ctmatrix, 2, sum) - 2 * diag(ctmatrix)
  return(res)
}

overallExchangeD <- function(ctmatrix){
  Exchange <- exchangeDj(ctmatrix)
  res <- sum(Exchange) / 2
  return(res)
}

overallQtyD <- function(ctmatrix){
  qtydj <- abs(apply(ctmatrix, 1, sum) - apply(ctmatrix, 2, sum))
  res <- sum(qtydj) / 2
  return(res)
}

overallShiftD <- function(ctmatrix){
  Shift <- shiftDj(ctmatrix)
  res <- sum(Shift) / 2
  return(res)
}

quantityDj <- function(ctmatrix){
  res <- abs(apply(ctmatrix, 1, sum) - apply(ctmatrix, 2, sum))
  return(res)
}

shiftDj <- function(ctmatrix){
  res <- overallDiffCatj(ctmatrix) - quantityDj(ctmatrix) - exchangeDj(ctmatrix)
  return(res)
}

omissionj <- function(ctmatrix){
  res <- apply(ctmatrix, 2, sum) - diag(ctmatrix)
  return(res)
}

comissionj <- function(ctmatrix){
  res <- apply(ctmatrix, 1, sum) - diag(ctmatrix)
  names(res) <- colnames(ctmatrix)
  return(res)
}

agreementj <- function(ctmatrix){
  res <- apply(ctmatrix, 1, sum) - comissionj(ctmatrix)
  names(res) <- colnames(ctmatrix)
  return(res)
}

