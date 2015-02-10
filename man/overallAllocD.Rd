\name{overallAllocD}
\alias{overallAllocD}

\title{
calculates overall allocation difference from a square contingency table
}
\description{
calculates overall allocation difference from a contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). Allocation difference is defined as the amount of difference between a reference variable and a comparison variable that is due to the less than maximum match in the spatial allocation of the categories, given the proportions of the categories in the reference and comparison variables. Allocation difference is equivalent to the addition of the exchange and shift components of difference (i.e., allocation difference can be disaggregated into exchange and shift components).
}
\usage{
overallAllocD(ctmatrix)
}

\arguments{
  \item{ctmatrix}{
matrix representing a square contingency table between a comparison variable (rows) and a reference variable (columns)
}
}
\value{
overall allocation difference between the comparison variable and the reference variable. Output values are given in the same units as \code{ctmatrix} 
}
\references{
Pontius Jr., R.G., Millones, M. 2011. \emph{Death to Kappa: birth of quantity disagreement and allocation disagreement for accuracy assessment}. International Journal of Remote Sensing 32 (15), 4407-4429.

Pontius Jr., R.G., Santacruz, A. 2014. \emph{Quantity, exchange and shift components of difference in a square contingency table}. International Journal of Remote Sensing 35 (21), 7543-7554.
}
\seealso{
\code{\link{overallQtyD}}, \code{\link{overallExchangeD}}, \code{\link{overallShiftD}}
}
\examples{
comp <- raster(system.file("external/comparison.rst", package="diffeR"))
ref <- raster(system.file("external/reference.rst", package="diffeR"))
ctmatCompRef <- crosstabm(comp, ref)
overallAllocD(ctmatCompRef)
}

\keyword{ spatial }
