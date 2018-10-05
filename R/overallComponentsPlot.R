overallComponentsPlot <- function(comp = NULL, ref = NULL, ctmatrix = NULL){
  
  if(!is.null(ctmatrix)){
    if(!is(ctmatrix, "matrix")) stop("ctmatrix must be of class matrix") 
    resT <- as.data.frame(cbind(0, overallQtyD(ctmatrix), overallExchangeD(ctmatrix), overallShiftD(ctmatrix), overallDiff(ctmatrix)))
    colnames(resT) <- c("Resolution", "Quantity", "Exchange", "Shift", "Overall")
  }
  
  if(!is.null(comp) & !is.null(ref)){
    resT <- differenceMR(comp, ref, eval = "original")
  }
  
  old.par <- par(no.readonly = TRUE)
  par(oma = c(0, 0, 0, 14))
  
  barplot(t(resT[2:4]), ylab = "Difference Size (percentage of domain)")
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend(x = 0.1, y = 0.3, c(colnames(resT)[4:2]), bty = "n", 
         fill = c(rgb(230,230,230, maxColorValue=255), rgb(174,174,174, maxColorValue=255), 
                  rgb(77,77,77, maxColorValue=255)))
  
  par(old.par)
}