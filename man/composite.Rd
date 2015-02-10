\name{composite}
\alias{composite}

\title{
create a composite matrix 
}
\description{
provide a method to create a composite matrix from the crosstabulation of a comparison map (or map at time \emph{t}) and a reference map (or map at time \emph{t}+1), both aggregated at a given factor
}
\usage{
composite(comp, ref, factor)
}

\arguments{
  \item{comp}{
object of class RasterLayer corresponding to a comparison map (or map at time \emph{t})
}
  \item{ref}{
object of class RasterLayer corresponding to a reference map (or map at time \emph{t}+1)
}
  \item{factor}{
integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). Or two integers (horizontal and vertical aggregation factor). See \code{raster} package for details
}
}
\details{
the pixel definition in a composite matrix interpretes class membership as the proportion of a pixel that belongs to a class. The pixel contains information about only the quantity of each category (Kuzera and Pontius 2008).
}
\value{
a matrix showing the contingency table derived from the crosstabulation of a comparison map (or map at time \emph{t}) and a reference map (or map at time \emph{t}+1), both aggregated at a given factor. Output values are given as proportion (0 to 1)
}
\references{
Kuzera, K., Pontius Jr., R.G. 2008. \emph{Importance of matrix construction for multiple-resolution categorical map comparison}. GIScience & Remote Sensing 45 (3), 249-274.
}
\seealso{
\code{\link{memberships}}
}
\examples{
comp <- raster(system.file("external/comparison.rst", package="diffeR"))
ref <- raster(system.file("external/reference.rst", package="diffeR"))
composite(comp, ref, factor=2)
}

\keyword{ spatial }
