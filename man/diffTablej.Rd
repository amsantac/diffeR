\name{diffTablej}
\alias{diffTablej}

\title{
calculates difference metrics at the category level from a square contingency table
}
\description{
calculates quantity, exchange and shift components of difference, as well as the overall difference, at the category level from a contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1).  

Quantity difference is defined as the amount of difference between the reference variable and a comparison variable that is due to the less than maximum match in the proportions of the categories. Exchange consists of a transition from category \emph{i} to category \emph{j} in some observations and a transition from category \emph{j} to category \emph{i} in an identical number of other observations. Shift refers to the difference remaining after subtracting quantity difference and exchange from the overall difference.
}
\usage{
diffTablej(ctmatrix, digits = 0)
}
\arguments{
  \item{ctmatrix}{
matrix representing a square contingency table between a comparison variable (rows) and a reference variable (columns)
}
  \item{digits}{	
integer indicating the number of decimal places to be used
}
}
\value{
data.frame containing difference metrics at the category level between a comparison variable (rows) and a reference variable (columns). Output values are given in the same units as \code{ctmatrix}
}
\references{
Pontius Jr., R.G., Millones, M. 2011. \emph{Death to Kappa: birth of quantity disagreement and allocation disagreement for accuracy assessment}. International Journal of Remote Sensing 32 (15), 4407-4429.

Pontius Jr., R.G., Santacruz, A. 2014. \emph{Quantity, exchange and shift components of difference in a square contingency table}. International Journal of Remote Sensing 35 (21), 7543-7554.
}
\seealso{
\code{\link{differenceMetrics}}
}
\examples{
comp <- raster(system.file("external/comparison.rst", package="diffeR"))
ref <- raster(system.file("external/reference.rst", package="diffeR"))
ctmatCompRef <- crosstabm(comp, ref)
diffTablej(ctmatCompRef)

# Adjustment to population assumming a stratified random sampling
(population <- matrix(c(1,2,3,2000,4000,6000), ncol=2))
ctmatCompRef <- crosstabm(comp, ref, percent=TRUE, population = population)
diffTablej(ctmatCompRef)
}

\keyword{ spatial }
