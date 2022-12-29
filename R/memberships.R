memberships <- function(grid, fact = 2){
  layers <- segregate(grid)
  membership <- aggregate(layers, fact=fact, fun=mean)
  return(membership) 
}