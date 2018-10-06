diffTablej <- function(ctmatrix, digits = 0){
  
  if(!is(ctmatrix, "matrix")) stop("ctmatrix must be of class matrix")
  
  Omission <- omissionj(ctmatrix)
  Agreement <- agreementj(ctmatrix)
  Comission <- comissionj(ctmatrix)
  Quantity <- quantityDj(ctmatrix)
  Exchange <- exchangeDj(ctmatrix)
  Shift <- shiftDj(ctmatrix)
  
  resT <- as.data.frame(cbind(Omission, Agreement, Comission, Quantity, Exchange, Shift))
  Overall <- colSums(resT)
  Overall[c("Quantity", "Exchange", "Shift")] <- Overall[c("Quantity", "Exchange", "Shift")] / 2
  
  resT <- round(rbind(resT, Overall), digits)
  resT <- cbind(c(colnames(ctmatrix), "Overall"), resT)
  colnames(resT)[1] <- "Category"
  rownames(resT) <- NULL
  return(resT)
}