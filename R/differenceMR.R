differenceMR <- function(comp, ref, eval = "multiple", percent = TRUE, fact = 2, population = NULL){
  
  # create cross-tabulation matrix
  ctmatrix <- crosstabm(comp, ref, percent=TRUE, population=population)
  
  # calculate various difference measures for original resolution
  if(eval=="original"){
    resT <- as.data.frame(cbind(res(comp)[1], overallQtyD(ctmatrix), overallExchangeD(ctmatrix), overallShiftD(ctmatrix), overallDiff(ctmatrix)))
    colnames(resT) <- c("Resolution", "Quantity", "Exchange", "Shift", "Overall")
    # convert difference measures to percentages if percent argument is FALSE
    if(percent == FALSE) resT[,2:5] <- resT[,2:5]/100
  }
  
  # calculate various difference measures for multiple resolutions
  if(eval=="multiple"){
    # calculate difference measures at original resolution
    res1 <- cbind(overallQtyD(ctmatrix), overallExchangeD(ctmatrix), overallShiftD(ctmatrix), overallDiff(ctmatrix))
    
    # determine maximum number of resolutions
    mdim <- max(ncol(comp), nrow(comp))
    maxp <- floor(log(mdim, fact))
    if((log(mdim, fact) - round(log(mdim, fact), 0)) != 0) maxp <- maxp+1
    factvect <-  c(1, fact^(1:maxp))
    
    # adjust maximum number of resolutions if necessary
    if(factvect[length(factvect)] > mdim) factvect[length(factvect)] <- mdim
    
    # create empty data frame for storing results
    resa <- data.frame(matrix(nrow=length(factvect), ncol=5))
    colnames(resa) <- c("Resolution", "Quantity", "Exchange", "Shift", "Overall")
    
    # store resolution values in first column of resa
    resa[,1] <- factvect*res(comp)[1]
    # store difference measures at original resolution in resa
    resa[1,2:5] <- res1
    
    # calculate difference measures at multiple resolutions
    for(i in 2:length(factvect)){
      factor <- factvect[i]
      # create composite map at specified resolution
      ctmatrix <- composite(comp, ref, factor)*100
      # calculate difference measures for composite map
      resi <- cbind(overallQtyD(ctmatrix), overallExchangeD(ctmatrix), overallShiftD(ctmatrix), overallDiff(ctmatrix))
      # store difference measures in resa
      resa[i,2:5] <- resi
    }
    
    # convert difference measures to percentages if percent argument is FALSE
    if(percent==FALSE) resa[,2:5] <- resa[,2:5]/100
    # combine resolution values and difference measures in a single data frame
    resT <- cbind(Multiples=factvect, resa)
  }
  # return results
  return(resT)
}
