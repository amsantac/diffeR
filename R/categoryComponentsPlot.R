categoryComponentsPlot <- function(comp = NULL, ref = NULL, ctmatrix = NULL, units = NULL, population = NULL, 
                                   fontSize = NULL, breaks = waiver(), labels = waiver(), limits = NULL){
  
  ylab <- ifelse(is.null(ctmatrix), "Difference Size (percentage of domain)", 
                 ifelse(is.null(units), "Difference Size (units)", paste0("Difference Size (", units, ")"))) 
  
  if(!is.null(comp) & !is.null(ref)){
    ctmatrix <- crosstabm(comp, ref, percent = TRUE, population = NULL)
  }
  
  if(!is.null(ctmatrix)){
    if(!is(ctmatrix, "matrix")) stop("ctmatrix must be of class matrix")
    if(!is.null(population)) ctmatrix <- sample2pop(ctmatrix, population)
    resT <- data.frame(Shift = shiftDj(ctmatrix), Exchange = exchangeDj(ctmatrix), Quantity = quantityDj(ctmatrix), 
                       Category = colnames(ctmatrix), stringsAsFactors = FALSE)
    rownames(resT) <- NULL
  }
  
  resT2 <- melt(resT, id.var = "Category", variable.name = "differenceType")

  ggplot() + geom_bar(aes_(x = ~Category, y = ~value, fill = ~differenceType), colour="black", data = resT2, stat = "identity") + 
    labs(x = "Category", y = ylab, fill = '') + theme_classic() + 
    scale_y_continuous(expand = c(0, 0), breaks = breaks, labels = labels, limits = limits) +
    theme(legend.position="top") + coord_flip() + guides(fill = guide_legend(reverse = TRUE)) +
    # scale_fill_manual(values=c("#ff0000", "#0000ff", "#008000"))
    scale_fill_manual(values=c("#e6e6e6", "#aeaeae", "#4d4d4d")) + theme(text = element_text(size = fontSize))
}
