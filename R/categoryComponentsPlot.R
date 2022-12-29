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
    resT <- data.frame(Shift = shiftDj(ctmatrix), Exchange = exchangeDj(ctmatrix), 
                       Quantity = quantityDj(ctmatrix), Category = colnames(ctmatrix))
    rownames(resT) <- NULL
  }
  
  resT2 <- pivot_longer(resT, cols = 1:3, names_to = "differenceType", values_to = "value")
  resT2$differenceType <- factor(resT2$differenceType, levels = c('Shift', 'Exchange', 'Quantity'))

  ggplot(data = resT2, 
         aes(x = .data$Category, y = .data$value, fill = .data$differenceType)) + 
    geom_bar(colour = "black", stat = "identity") + 
    labs(x = "Category", y = ylab, fill = '') + 
    theme_classic() + 
    scale_y_continuous(expand = c(0, 0), breaks = breaks, labels = labels, limits = limits) +
    theme(legend.position = "top") + 
    coord_flip() + 
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = c(Shift = "#e6e6e6", Exchange = "#aeaeae", Quantity = "#4d4d4d")) + 
    theme(text = element_text(size = fontSize))
}
