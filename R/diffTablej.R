diffTablej <- function(ctmatrix, digits = 0, analysis = "error"){
  
  # check that ctmatrix is a matrix
  if(!is(ctmatrix, "matrix")) stop("ctmatrix must be of class matrix")
  
  # calculate omission
  Omission <- omissionj(ctmatrix)
  # calculate agreement
  Agreement <- agreementj(ctmatrix)
  # calculate commission
  Comission <- comissionj(ctmatrix)
  # calculate differences of quantity
  Quantity <- quantityDj(ctmatrix)
  # calculate differences of exchange
  Exchange <- exchangeDj(ctmatrix)
  # calculate differences of shift
  Shift <- shiftDj(ctmatrix)
  
  # create data frame of all difference types
  resT <- as.data.frame(cbind(Omission, Agreement, Comission, Quantity, Exchange, Shift))
  # calculate total differences for each category
  Overall <- colSums(resT)
  # divide Quantity, Exchange, and Shift differences by 2 to account for double counting
  Overall[c("Quantity", "Exchange", "Shift")] <- Overall[c("Quantity", "Exchange", "Shift")] / 2
  
  # round results to specified number of digits
  resT <- round(rbind(resT, Overall), digits)
  # add column names to data frame
  resT <- cbind(c(colnames(ctmatrix), "Overall"), resT)
  colnames(resT)[1] <- "Category"
  # change column names if analysis is "change"
  if(analysis == "change") colnames(resT)[2:4] <- c("Gain", "Persistence", "Loss")
  # remove row names from data frame
  rownames(resT) <- NULL
  # return resulting data frame
  return(resT)
}
