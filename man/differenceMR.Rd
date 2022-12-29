\name{differenceMR}
\alias{differenceMR}

\title{
calculates difference metrics between a reference map and a comparison map both consecutively aggregated at multiple resolutions
}
\description{
calculates quantity, exchange and shift components of difference, as well as the overall difference, between a comparison raster map (or map at time \emph{t}), and a reference raster map (or map at time \emph{t}+1), both consecutively aggregated at multiple resolutions.  

Quantity difference is defined as the amount of difference between the reference map and a comparison map that is due to the less than maximum match in the proportions of the categories. Exchange consists of a transition from category \emph{i} to category \emph{j} in some pixels and a transition from category \emph{j} to category \emph{i} in an identical number of other pixels. Shift refers to the difference remaining after subtracting quantity difference and exchange from the overall difference.
}
\usage{
differenceMR(comp, ref, eval = "multiple", percent = TRUE, fact = 2, 
             population = NULL)
}

\arguments{
  \item{comp}{
object of class SpatRaster corresponding to the comparison map, or map at time \emph{t}
}
  \item{ref}{
object of class SpatRaster corresponding to the reference map, or map at time \emph{t}+1
}
  \item{eval}{
default "original", return difference metrics between the input raster maps at the original resolution; if "multiple", return difference metrics at multiple resolutions aggregated according to a geometric sequence 
}
  \item{percent}{
logical. If TRUE, output value is given as percentage. If FALSE, output value is given as proportion (0 to 1)
}
  \item{fact}{
positive integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). Or two integers (horizontal and vertical aggregation factor). See \code{aggregate()} in the \code{terra} package for details
}
  \item{population}{
an \emph{n} x 2 matrix provided to correct the sample count to population count in the square contingency table. See Details below
}
}
\details{
For correcting the sample count to population count in the square contingency table, assuming a stratified random sampling, an \emph{n} (number of categories) by 2 matrix can be provided in the \code{population} argument. The first column of \code{population} must contains integer identifiers of each category, corresponding to the categories in the comparison map (or map at time \emph{t}) and reference map (or map at time \emph{t}+1). The second column corresponds to the population totals for each map category  
}
\value{
data.frame containing quantity, exchange and shift components of difference, as well as the overall difference, between the comparison map and the reference map at multiple resolutions
}
\references{
Pontius Jr., R.G., Millones, M. 2011. \emph{Death to Kappa: birth of quantity disagreement and allocation disagreement for accuracy assessment}. International Journal of Remote Sensing 32 (15), 4407-4429.

Pontius Jr., R.G., Santacruz, A. 2014. \emph{Quantity, exchange and shift components of difference in a square contingency table}. International Journal of Remote Sensing 35 (21), 7543-7554.
}
\examples{
\dontrun{
comp <- rast(system.file("external/comparison.rst", package = "diffeR"))
ref <- rast(system.file("external/reference.rst", package = "diffeR"))
differenceMR(comp, ref, eval = "original")
differenceMR(comp, ref, eval = "multiple", fact = 2)
}
}

\keyword{ spatial }
