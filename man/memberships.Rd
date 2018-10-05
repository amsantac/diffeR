\name{memberships}
\alias{memberships}

\title{
produces membership values for each category in the input raster at a specified aggregated resolution 
}
\description{
Calculates membership values for each category in the input raster at a specified aggregated resolution
}
\usage{
memberships(grid, fact = 2)
}

\arguments{
  \item{grid}{
object of class RasterLayer
}
  \item{fact}{
integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). Or two integers (horizontal and vertical aggregation factor). See \code{raster} package for details
}
}
\value{
a RasterBrick object containing membership values for each category in the input raster at a specified aggregated resolution
}
\seealso{
\code{\link{composite}} 
}
\examples{
ref <- raster(system.file("external/reference.rst", package="diffeR"))
plot(ref)
memb.ref <- memberships(ref, fact=2)
names(memb.ref) <- c("ref.A", "ref.B", "ref.C")
plot(memb.ref)
}

\keyword{ spatial }
