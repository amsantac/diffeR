\name{overallDiff}
\alias{overallDiff}

\title{
calculates overall difference between from a square contingency table
}
\description{
calculates overall difference from a contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). Overall difference is equivalent to the addition of the quantity and allocation components of difference (i.e., overall difference can be disaggregated into quantity and allocation components). 
}
\usage{
overallDiff(ctmatrix)
}

\arguments{
  \item{ctmatrix}{
matrix representing a square contingency table between a comparison variable (rows) and a reference variable (columns)
}
}
\value{
overall difference between the comparison variable and the reference variable. Output values are given in the same units as \code{ctmatrix}
}
\references{
Pontius Jr., R.G., Millones, M. 2011. \emph{Death to Kappa: birth of quantity disagreement and allocation disagreement for accuracy assessment}. International Journal of Remote Sensing 32 (15), 4407-4429.

Pontius Jr., R.G., Santacruz, A. 2014. \emph{Quantity, exchange and shift components of difference in a square contingency table}. International Journal of Remote Sensing 35 (21), 7543-7554.
}
\seealso{
\code{\link{overallQtyD}}, \code{\link{overallAllocD}}
}
\examples{
comp <- raster(system.file("external/comparison.rst", package="diffeR"))
ref <- raster(system.file("external/reference.rst", package="diffeR"))
ctmatCompRef <- crosstabm(comp, ref)
overallDiff(ctmatCompRef)
}

\keyword{ spatial }
