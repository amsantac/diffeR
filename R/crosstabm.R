crosstabm <- function (comp, ref, percent = FALSE, population = NULL){
  
  # create cross-tabulation table of comp and ref
  cr1 <- crosstab(c(comp, ref), long = TRUE)
  
  # remove rows with missing values
  cr1 <- stats::na.omit(cr1) 
  
  # create vector of unique values in comp and ref
  uniquecr1 <- unique(c(cr1[, 1], cr1[, 2]))
  
  # create empty matrix for storing cross-tabulation counts
  SampleCount <- matrix(0, nrow = length(uniquecr1), ncol = length(uniquecr1))
  colnames(SampleCount) <- uniquecr1
  rownames(SampleCount) <- uniquecr1
  
  # fill in SampleCount matrix with counts from cr1
  for (i in 1:nrow(cr1)) {
    xi <- which(rownames(SampleCount) == cr1[i,1])
    ji <- which(colnames(SampleCount) == cr1[i,2])
    SampleCount[xi, ji] <- as.numeric(cr1[i, 3])
  }
  
  # convert counts to percentages if percent argument is TRUE
  if (percent == TRUE) {
    SampleCount <- SampleCount/sum(SampleCount) * 100
  }
  
  # if population size is specified, convert counts to proportions based on population size
  if(!is.null(population)){
    SampleCount <- sample2pop(SampleCount, population)
    
    # convert proportions to percentages if percent argument is TRUE
    if(percent == TRUE)
      SampleCount <- SampleCount/sum(SampleCount) * 100
  }
  
  # return modified SampleCount matrix
  return(SampleCount)
}
