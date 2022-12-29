categorySourcesPlot <- function(comp = NULL, ref = NULL, ctmatrix = NULL, analysis = "error", units = NULL, population = NULL, 
                                fontSize = NULL, breaks = waiver(), labels = waiver(), limits = NULL){
  
  ylab <- ifelse(is.null(ctmatrix), "Percentage of Domain", 
                 ifelse(is.null(units), "units", units)) 
  
  if(!is.null(comp) & !is.null(ref)){
    ctmatrix <- crosstabm(comp, ref, percent = TRUE, population = NULL)
  }
  
  if(!is.null(ctmatrix)){
    if(!is(ctmatrix, "matrix")) stop("ctmatrix must be of class matrix")
    if(!is.null(population)) ctmatrix <- sample2pop(ctmatrix, population)
    resT <- data.frame(Comission = comissionj(ctmatrix), Agreement = agreementj(ctmatrix), 
                       Omission = omissionj(ctmatrix), Category = colnames(ctmatrix))
    if(analysis == "change") colnames(resT)[1:3] <- c("Loss", "Persistence", "Gain")
    rownames(resT) <- NULL
  }
  
  resT2 <- pivot_longer(resT, cols = 1:3, names_to = "differenceType", values_to = "value")
  resT2$differenceType <- factor(resT2$differenceType, levels = c('Comission', 'Agreement', 'Omission'))

  ggplot(data = resT2, 
         aes(x = .data$Category, y = .data$value, fill = .data$differenceType)) + 
    geom_bar(colour = "black", stat = "identity") + 
    labs(x = "Category", y = ylab, fill = '') + 
    theme_classic() + 
    scale_y_continuous(expand = c(0, 0), breaks = breaks, labels = labels, limits = limits) +
    theme(legend.position = "top") + 
    coord_flip() + 
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = c(Comission = "#aeaeae", Agreement = "#4d4d4d", Omission = "#e6e6e6")) + 
    theme(text = element_text(size = fontSize))
}
