diffTablej <- function(ctmatrix, digits = 0){
  if(!is(ctmatrix, "matrix")) stop("ctmatrix must be of class matrix")
  Quantity <- quantityDj(ctmatrix)
  Exchange <- exchangeDj(ctmatrix)
  Shift <- shiftDj(ctmatrix)
  resT <- as.data.frame(cbind(Quantity, Exchange, Shift))
  Overall <- apply(resT, 2, sum) / 2
  resT <- round(rbind(resT, Overall), digits)
  resT <- cbind(c(colnames(ctmatrix), "Overall"), resT)
  colnames(resT) <- c("Category", "Quantity", "Exchange", "Shift")
  return(resT)
}