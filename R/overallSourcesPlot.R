overallSourcesPlot <- function(comp = NULL, ref = NULL, ctmatrix = NULL, units = NULL, population = NULL){
  
  ylab = "Difference Size (percentage of domain)"
  
  if(!is.null(comp) & !is.null(ref)){
    ctmatrix <- crosstabm(comp, ref, percent = TRUE, population = NULL)
  }
  
  if(!is.null(ctmatrix)){
    if(!is(ctmatrix, "matrix")) stop("ctmatrix must be of class matrix")
    if(!is.null(population)) ctmatrix <- sample2pop(ctmatrix, population)
    resT <- data.frame(Comission = comissionj(ctmatrix), Omission = omissionj(ctmatrix), Category = colnames(ctmatrix), 
                       stringsAsFactors = FALSE)
    rownames(resT) <- NULL
    ylab <- ifelse(is.null(units), "Difference Size (units)", paste0("Difference Size (", units, ")"))
  }
  
  resT2 <- melt(resT, id.var = "Category", variable.name = "differenceType")
  #resT2 <- gather(resT, differenceType, value, Comission:Omission)
  
  ggplot() + geom_bar(aes_(x = ~differenceType, y = ~value, fill = ~Category), data = resT2, stat = "identity") + 
    labs(x = "Type of Difference", y = ylab) + theme_classic() + scale_y_continuous(expand = c(0, 0))
}



