\name{crosstabm}
\alias{crosstabm}

\title{
create a contingency table between a comparison raster map (rows) and a reference raster map (columns)
}
\description{
create a contingency table, also called cross-tabulated matrix, between a comparison raster map (rows), or map at time \emph{t}, and a reference raster map (columns), or map at time \emph{t}+1
}
\usage{
crosstabm(comp, ref, percent = FALSE, population = NULL)
}

\arguments{
  \item{comp}{
object of class SpatRaster corresponding to the comparison map, or map at time \emph{t}
}
  \item{ref}{
object of class SpatRaster corresponding to the reference map, or map at time \emph{t}+1
}
  \item{percent}{
logical. If TRUE, output values are given as percentage. If FALSE, output values are given in pixel counts
}
  \item{population}{
an \emph{n} x 2 matrix provided to correct the sample count to population count in the square contingency table. See Details below
}
}
\details{
For correcting the sample count to population count in the square contingency table, assuming a stratified random sampling, an \emph{n} (number of categories) by 2 matrix can be provided in the \code{population} argument. The first column of \code{population} must contains integer identifiers of each category, corresponding to the categories in the comparison map (or map at time \emph{t}) and reference map (or map at time \emph{t}+1). The second column corresponds to the population totals for each map category  
}
\value{
a matrix showing the cross-tabulation between the comparison map (or map at time \emph{t}) and the reference map (or map at time \emph{t}+1)
}
\seealso{
\code{\link{memberships}}
}
\examples{
comp <- rast(system.file("external/comparison.rst", package = "diffeR"))
ref <- rast(system.file("external/reference.rst", package = "diffeR"))
crosstabm(comp, ref)

# Population-adjusted square contingency table
(population <- matrix(c(1, 2, 3, 2000, 4000, 6000), ncol = 2))
crosstabm(comp, ref, population = population)

# Population-adjusted square contingency table, output as percentage
crosstabm(comp, ref, percent = TRUE, population = population)
}

\keyword{ spatial }
