\name{quantityDj}
\alias{quantityDj}

\title{
calculates quantity difference at the category level from a square contingency table
}
\description{
calculates quantity difference at the category level from a contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). Quantity difference is defined as the amount of difference between the reference variable and a comparison variable that is due to the less than maximum match in the proportions of the categories. 
}
\usage{
quantityDj(ctmatrix)
}

\arguments{
  \item{ctmatrix}{
matrix representing a square contingency table between a comparison variable (rows) and a reference variable (columns)
}
}
\value{
a numeric vector containing the quantity difference between the comparison variable and the reference variable at the category level. Output values are given in the same units as \code{ctmatrix}
}
\references{
Pontius Jr., R.G., Millones, M. 2011. \emph{Death to Kappa: birth of quantity disagreement and allocation disagreement for accuracy assessment}. International Journal of Remote Sensing 32 (15), 4407-4429.

Pontius Jr., R.G., Santacruz, A. 2014. \emph{Quantity, exchange and shift components of difference in a square contingency table}. International Journal of Remote Sensing 35 (21), 7543-7554.
}
\seealso{
\code{\link{overallQtyD}}
}
\examples{
comp <- raster(system.file("external/comparison.rst", package="diffeR"))
ref <- raster(system.file("external/reference.rst", package="diffeR"))
ctmatCompRef <- crosstabm(comp, ref)
quantityDj(ctmatCompRef)
}

\keyword{ spatial }
