MADscatterplot <- function(grid1, grid2, strata = NULL){

	# if the user does not provide a strata map, create one filled with 1's
	if(is.null(strata)) {
		strata <- grid1
		# strata[] <- 1
		strata[!is.na(strata)] <- 1
	}
	
	# multiply the grids by the strata map, in case there are zeros
	# or NA values in the strata map that need to be excluded
	strata <- strata / strata
	grid1 <- grid1 * strata
	grid2 <- grid2 * strata
	
	# create the table with the values to plot and create the plot
	d <- data.frame(grid1 = values(grid1, mat = FALSE), 
	                grid2 = values(grid2, mat = FALSE), 
	                strata = values(strata, mat = FALSE)
	ggplot(d, aes(x = .data$grid1,y = .data$grid2)) + 
	  geom_point() + 
	  coord_fixed(ratio = 1) + 
	  geom_abline(colour = "red") + 
	  theme_bw()
}
