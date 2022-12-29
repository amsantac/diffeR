exchangeDij <- function(ctmatrix){
  # calculate pairwise exchange difference for all categories
  exchDij <- 2 * pmin(ctmatrix, t(ctmatrix))
  # select lower triangle of exchDij
  res <- lower.tri(exchDij) * exchDij
  # return lower triangle of exchDij
  return(res)
}

exchangeDj <- function(ctmatrix){
  # calculate pairwise exchange difference for all categories
  exchDij <- 2 * pmin(ctmatrix, t(ctmatrix))
  # select lower triangle of exchDij
  exchDij.lt <- lower.tri(exchDij) * exchDij
  # sum exchange differences for each category
  res <- apply(exchDij.lt, 1, sum) + apply(exchDij.lt, 2, sum)
  # return sum of exchange differences for each category
  return(res)
}

overallAllocD <- function(ctmatrix){
  # calculate overall allocation difference
  res <- overallDiff(ctmatrix) - overallQtyD(ctmatrix)
  # return overall allocation difference
  return(res)
}

overallDiff <- function(ctmatrix){
  # calculate overall difference
  res <- sum(ctmatrix) - sum(diag(ctmatrix))
  # return overall difference
  return(res)
}

overallDiffCatj <- function(ctmatrix){
  # calculate overall difference for each category
  res <- apply(ctmatrix, 1, sum) + apply(ctmatrix, 2, sum) - 2 * diag(ctmatrix)
  # return overall difference for each category
  return(res)
}

overallExchangeD <- function(ctmatrix){
  # calculate overall exchange difference
  Exchange <- exchangeDj(ctmatrix)
  res <- sum(Exchange) / 2
  # return overall exchange difference
  return(res)
}

overallQtyD <- function(ctmatrix){
  # calculate overall quantity difference
  qtydj <- abs(apply(ctmatrix, 1, sum) - apply(ctmatrix, 2, sum))
  res <- sum(qtydj) / 2
  # return overall quantity difference
  return(res)
}

overallShiftD <- function(ctmatrix){
  # calculate overall shift difference
  Shift <- shiftDj(ctmatrix)
  res <- sum(Shift) / 2
  # return overall shift difference
  return(res)
}

quantityDj <- function(ctmatrix){
  # calculate quantity difference for each category
  res <- abs(apply(ctmatrix, 1, sum) - apply(ctmatrix, 2, sum))
  # return quantity difference for each category
  return(res)
}

shiftDj <- function(ctmatrix){
  # calculate shift difference for each category
  res <- overallDiffCatj(ctmatrix) - quantityDj(ctmatrix) - exchangeDj(ctmatrix)
  # return shift difference for each category
  return(res)
}

omissionj <- function(ctmatrix){
  # calculate omission for each category
  res <- apply(ctmatrix, 2, sum) - diag(ctmatrix)
  # return omission for each category
  return(res)
}

comissionj <- function(ctmatrix){
  # calculate commission for each category
  res <- apply(ctmatrix, 1, sum) - diag(ctmatrix)
  # assign names to commission values
  names(res) <- colnames(ctmatrix)
  # return commission for each category
  return(res)
}

agreementj <- function(ctmatrix){
  # calculate agreement for each category
  res <- apply(ctmatrix, 1, sum) - comissionj(ctmatrix)
  # assign names to agreement values
  names(res) <- colnames(ctmatrix)
  # return agreement for each category
  return(res)
}
