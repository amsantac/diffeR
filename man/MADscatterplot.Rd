\name{MADscatterplot}
\alias{MADscatterplot}

\title{
MAD scatterplot
}
\description{
Generates a scatterplot indicating the distribution of values from two images in relation to the 1:1 line
}
\usage{
MADscatterplot(grid1, grid2, strata = NULL)
}

\arguments{
  \item{grid1}{
object of class RasterLayer corresponding to the first image 
}
  \item{grid2}{
object of class RasterLayer corresponding to the second image 
}
  \item{strata}{
object of class RasterLayer corresponding to the mask or strata image. Zero values are taken as no data (i.e., NA) 
}
}
\value{
a ggplot object corresponding to the scatterplot
}
\seealso{
\code{\link{MAD}} 
}
\examples{
old.par <- par(no.readonly = TRUE)
grid1 <- raster(system.file("external/GRID1_INT.rst", package="diffeR"))
grid2 <- raster(system.file("external/GRID2_INT.rst", package="diffeR"))
strata <- raster(system.file("external/strata_int.rst", package="diffeR"))
MADscatterplot(grid1, grid2, strata)

veg_obs1 <- raster(system.file("external/veg_obs1.rst", package="diffeR")) 
veg_pre1 <- raster(system.file("external/veg_pre1.rst", package="diffeR"))
veg_mask1 <- raster(system.file("external/veg_mask1.rst", package="diffeR")) 
MADscatterplot(veg_obs1, veg_pre1, veg_mask1) 
par(old.par)
}

\keyword{ spatial }