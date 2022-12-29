sample2pop <- function(ctmatrix, population){
  
  # check that ctmatrix has defined column and row names
  if(is.null(colnames(ctmatrix)) | is.null(rownames(ctmatrix)))
    stop("colnames and rownames of ctmatrix must be defined as integer identifiers to match with first column of population")
  
  # check that population has two columns
  if ((dim(population)[2] != 2)) 
    stop("population must have two columns")
  
  # check that ctmatrix is a square matrix
  if ((dim(ctmatrix)[1] != dim(ctmatrix)[2])) 
    stop("ctmatrix must be a square matrix")
  
  # check that number of rows in population and ctmatrix are equal
  if ((dim(population)[1] != dim(ctmatrix)[1])) 
    stop("number of rows of population and ctmatrix must be equal")
  
  # convert counts in ctmatrix to proportions based on population size
  for (i in 1:dim(population)[1]){
    j <- which(colnames(ctmatrix) == population[i, 1])
    ctmatrix[j,] <- population[j, 2]*ctmatrix[j,]/sum(ctmatrix[j,])
  }
  
  # return modified ctmatrix
  return(ctmatrix)  
}

