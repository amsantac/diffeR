\name{overallShiftD}
\alias{overallShiftD}

\title{
calculates overall shift difference from a square contingency table
}
\description{
calculates overall shift difference from a contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1).
Shift refers to the difference remaining after subtracting quantity difference and exchange from the overall difference. 
}
\usage{
overallShiftD(ctmatrix)
}

\arguments{
  \item{ctmatrix}{
matrix representing a square contingency table between a comparison variable (rows) and a reference variable (columns)
}
}
\value{
overall shift difference between the comparison variable and the reference variable. Output values are given in the same units as \code{ctmatrix} 
}
\references{
Pontius Jr., R.G., Santacruz, A. 2014. \emph{Quantity, exchange and shift components of difference in a square contingency table}. International Journal of Remote Sensing 35 (21), 7543-7554.
}
\seealso{
\code{\link{overallAllocD}}
}
\examples{
comp <- raster(system.file("external/comparison.rst", package="diffeR"))
ref <- raster(system.file("external/reference.rst", package="diffeR"))
ctmatCompRef <- crosstabm(comp, ref)
overallShiftD(ctmatCompRef)
}

\keyword{ spatial }
