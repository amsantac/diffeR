overallComponentsPlot <- function(comp = NULL, ref = NULL, ctmatrix = NULL, units = NULL, population = NULL){
  
  ylab <- "Difference Size (percentage of domain)"
  
  if(!is.null(comp) & !is.null(ref)){
    resT <- differenceMR(comp, ref, eval = "original", population = population)[, c("Quantity", "Exchange", "Shift")]
  }
  
  if(!is.null(ctmatrix)){
    if(!is(ctmatrix, "matrix")) stop("ctmatrix must be of class matrix")
    if(!is.null(population)) ctmatrix <- sample2pop(ctmatrix, population)
    resT <- data.frame(Quantity = overallQtyD(ctmatrix), Exchange = overallExchangeD(ctmatrix), Shift = overallShiftD(ctmatrix))
    ylab <- ifelse(is.null(units), "Difference Size (units)", paste0("Difference Size (", units, ")"))
  }
  
  old.par <- par(no.readonly = TRUE)
  par(oma = c(0, 0, 0, 14))
  
  barplot(t(resT), ylab = ylab)
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend(x = 0.1, y = 0.3, c(colnames(resT)[3:1]), bty = "n", 
         fill = c("#e6e6e6", "#aeaeae", "#4d4d4d"))
  par(old.par)
}