\name{MAD}
\alias{MAD}

\title{
Mean Absolute Deviation (MAD) 
}
\description{
Provides a method to compare the quantity difference and allocation difference between two images of the same real variable at the original resolution or at multiple resolutions.  
The output provides a stacked graph and an accompanying numerical table for the Mean Absolute Deviation (MAD) for the difference due to quantity, the difference due to stratum-level allocation, and difference due to pixel-level allocation.  The output also indicates which image has a smaller average.  A scatterplot indicating the distribution of values in relation to the 1:1 line can be produced with \code{\link{MADscatterplot}}
}
\usage{
MAD(grid1, grid2, strata = NULL, eval = "original")
}

\arguments{
  \item{grid1}{
object of class SpatRaster corresponding to the first image
}
  \item{grid2}{
object of class SpatRaster corresponding to the second image
}
  \item{strata}{
object of class SpatRaster corresponding to the mask or strata image. Zero values are taken as no data (i.e., NA)
}
  \item{eval}{
default "original", return the MAD value for the original resolution; if "multiple", return the MAD values for multiple resolutions following a geometric sequence 
}
}
\value{
a dataframe containing the multiples of the original resolution, the corresponding aggregated resolution, the difference due to quantity, the difference due to stratum-level allocation, and the difference due to pixel-level allocation.
}
\references{
Pontius Jr., R.G., Thontteh, O., Chen, H. 2008. \emph{Components of information for multiple resolution comparison between maps that share a real variable}. Environmental and Ecological Statistics 15 (2), 111-142. 
}
\seealso{
\code{\link{MADscatterplot}}
}
\examples{
old.par <- par(no.readonly = TRUE)
grid1 <- rast(system.file("external/GRID1_INT.rst", package = "diffeR"))
grid2 <- rast(system.file("external/GRID2_INT.rst", package = "diffeR"))
strata <- rast(system.file("external/strata_int.rst", package = "diffeR"))
MAD(grid1, grid2, strata, eval = "original")
MAD(grid1, grid2, strata, eval = "multiple")

\dontrun{
veg_obs1 <- rast(system.file("external/veg_obs1.rst", package = "diffeR")) 
veg_pre1 <- rast(system.file("external/veg_pre1.rst", package = "diffeR")) 
veg_mask1 <- rast(system.file("external/veg_mask1.rst", package = "diffeR")) 
MADscatterplot(veg_obs1, veg_pre1, veg_mask1) 
MAD(veg_obs1, veg_pre1, veg_mask1, eval = "multiple") 
}
par(old.par)
}

\keyword{ spatial }
