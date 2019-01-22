crosstabm <- function (comp, ref, percent = FALSE, population = NULL) 
{
  # suggested by R. Hijmans for compatibility with new version of the 'raster' package
  #cr1 <- crosstab(comp, ref)
  cr1 <- crosstab(comp, ref, long = TRUE)
  
  cr1 <- na.omit(cr1) # should be the same as below, but more concise
  #if (any(is.na(cr1[, 1]))) {
  #    cr1 <- cr1[-which(is.na(cr1[, 1])), ]
  #}
  #if (any(is.na(cr1[, 2]))) {
  #    cr1 <- cr1[-which(is.na(cr1[, 2])), ]
  #}
  
  #uniquecr1 <- unique(c(levels(cr1[, 1]), levels(cr1[, 2])))
  uniquecr1 <- unique(c(cr1[, 1], cr1[, 2]))
  SampleCount <- matrix(0, nrow = length(uniquecr1), ncol = length(uniquecr1))
  colnames(SampleCount) <- uniquecr1
  rownames(SampleCount) <- uniquecr1
  for (i in 1:nrow(cr1)) {
    xi <- which(rownames(SampleCount) == cr1[i,1])
    ji <- which(colnames(SampleCount) == cr1[i,2])
    SampleCount[xi, ji] <- as.numeric(cr1[i, 3])
  }
  if (percent == TRUE) {
    SampleCount <- SampleCount/sum(SampleCount) * 100
  }
  
  if(!is.null(population)){
    SampleCount <- sample2pop(SampleCount, population)
    if(percent == TRUE)
      SampleCount <- SampleCount/sum(SampleCount) * 100
  }
  return(SampleCount)
}
