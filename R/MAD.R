MAD <- function(grid1, grid2, strata = NULL, eval = "original"){
  
  # if the user does not provide a strata map, create one filled with 1's
  if(is.null(strata)) {
    strata <- grid1
    strata[!is.na(strata)] <- 1
  }
  # calculation of deviations
  dev <- grid1 - grid2
  
  # strataM <- stack(strata)
  strataM <- strata
  
  # if there are 0's in the strata map, the 0's are converted to 
  # NA values
  if(any(unique(strata) == 0)) {
    fun <- function(x) { x[x == 0] <- NA; return(x) }
    x <- app(strata, fun)
    # strataM <- stack(x)
    strataM <- x
  }
  
  # for each stratum, a boolean map is created
  # it produces a multilayer raster, each stratum in a different layer
  strataM <- segregate(strataM)
  
  # if there are NA values in the strata map, that layer is dropped
  # if(any(is.na(unique(strataM)))) strataM <- dropLayer(strataM, which(is.na(unique(strataM))))
  
  # Count of nozeros and no NA values
  nozeros <- global(app(strataM, sum), sum, na.rm = TRUE)[1, 1]
  
  # the deviation map is multiplied by the boolean maps
  MADe <- dev * strataM
  
  # calculation of MAD quantity for the finest resolution
  PerfMed <- abs(sum(global(MADe, sum, na.rm = TRUE)/nozeros))
  # calculation of MAD strata for the finest resolution
  MADestr <- sum(abs(global(MADe, sum, na.rm = TRUE)))/nozeros - PerfMed
  # calculation of MAD total for the finest resolution
  MADTot1 <- sum(global(abs(MADe), sum, na.rm = TRUE))/nozeros
  
  # calculation of the factors of aggregation in powers of 2
  mdim <- max(ncol(grid1), nrow(grid1))
  maxp <- floor(log(mdim, 2))
  if((log(mdim, 2) - round(log(mdim, 2), 0)) != 0) maxp <- maxp + 1
  factvect <- c(1, 2^(1:maxp))
  
  # matrix for storing the results
  resa <- data.frame(matrix(nrow = length(factvect), ncol = 5))
  colnames(resa) <- c("Resolution", "Quantity", "Strata", "Element", "Total")
  
  # the first column of the matrix : resolutions
  resa[,1] <- factvect*res(grid1)[1]
  
  # quantity, strata and total MAD for the finest resolution are stored
  # in the results matrix
  resa[1,2] <- PerfMed
  resa[1,3] <- MADestr
  resa[1,5] <- MADTot1
  resa[1,4] <- resa[1,5] - resa[1,2] - resa[1,3] 
  
  # if the user wants multiple evaluations following the geometric sequence
  if(eval == "multiple"){
    for(i in 2:length(factvect)){
      fact <- factvect[i]
      # aggregate the map and calculate the MAD components			
      suppressWarnings(MADea <- aggregate(MADe, fact = fact, fun = sum, na.rm = TRUE))
      PerfMed <- abs(sum(global(MADea, sum, na.rm = TRUE)/nozeros))
      resa[i,2] <- PerfMed
      MADestr <- sum(abs(global(MADea, sum, na.rm = TRUE)))/nozeros - PerfMed
      resa[i,3] <- MADestr
      MADTot <- sum(global(abs(MADea), sum, na.rm = TRUE))/nozeros
      resa[i,5] <- MADTot
    }
    
    # MAD element is calculated as the total minus quantity minus strata
    resa[,4] <- resa[,5] - resa[,2] - resa[,3] 
    
    # add the column with the index of multiple resolutions
    resa <- cbind(Multiples = factvect, resa)
    
    # code for the plot
    # create the table with the values to plot and create the plot
    eg3 <- data.frame(rep(1:nrow(resa), each = 3), 
                      rep(c("Quantity", "Strata", "Element"), nrow(resa)), 
                      c(t(resa[,3:5])))
    colnames(eg3) <- c("Resolution", "Component", "MAD")

    p <- ggplot() + 
      theme_bw() + 
      geom_area(data = eg3, aes_(x = ~Resolution, y = ~MAD, group = ~Component, fill = ~Component), position="stack") + 
      scale_x_continuous(breaks = c(1:nrow(resa)), labels = c(resa$Multiples)) + 
      labs(x = "Resolution (Multiples of initial)", 
           y = "Mean Absolute Deviation (MAD)")
    print(p)
  }
  
  # print the message indicating which grid has the lower mean
  ifelse(mean(values(grid1, mat = FALSE), na.rm = TRUE) < mean(values(grid2, mat = FALSE), na.rm = TRUE), 
         print(paste("The mean of grid1 is less than the mean of grid2")), 
         print(paste("The mean of grid2 is less than the mean of grid1"))) 
  
  # return the dataframe with the results
  ifelse(eval == "multiple", return(resa), return(resa[1,]))
}
