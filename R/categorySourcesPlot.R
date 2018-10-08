categorySourcesPlot <- function(comp = NULL, ref = NULL, ctmatrix = NULL, analysis = "error", units = NULL, population = NULL){
  
  ylab <- ifelse(is.null(ctmatrix), "Percentage of Domain", 
                 ifelse(is.null(units), "units", units)) 
  
  if(!is.null(comp) & !is.null(ref)){
    ctmatrix <- crosstabm(comp, ref, percent = TRUE, population = NULL)
  }
  
  if(!is.null(ctmatrix)){
    if(!is(ctmatrix, "matrix")) stop("ctmatrix must be of class matrix")
    if(!is.null(population)) ctmatrix <- sample2pop(ctmatrix, population)
    resT <- data.frame(Comission = comissionj(ctmatrix), Agreement = agreementj(ctmatrix), Omission = omissionj(ctmatrix), 
                       Category = colnames(ctmatrix), stringsAsFactors = FALSE)
    if(analysis == "change") colnames(resT)[1:3] <- c("Loss", "Persistence", "Gain")
    rownames(resT) <- NULL
  }
  
  resT2 <- melt(resT, id.var = "Category", variable.name = "differenceType")

  ggplot() + geom_bar(aes_(x = ~Category, y = ~value, fill = ~differenceType), colour="black", data = resT2, stat = "identity") + 
    labs(x = "Category", y = ylab, fill = '') + theme_classic() + scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position="top") + coord_flip() + guides(fill = guide_legend(reverse = TRUE)) +
    # scale_fill_manual(values=c("#ff0000", "#0000ff", "#008000"))
    scale_fill_manual(values=c("#aeaeae", "#4d4d4d", "#e6e6e6"))
}