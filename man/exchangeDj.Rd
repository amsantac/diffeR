\name{exchangeDj}
\alias{exchangeDj}

\title{
calculates exchange difference at the category level from a square contingency table
}
\description{
calculates exchange difference at the category level from a contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1).
Exchange consists of a transition from category \emph{i} to category \emph{j} in some observations and a transition from category \emph{j} to category \emph{i} in an identical number of other observations.  
}
\usage{
exchangeDj(ctmatrix)
}

\arguments{
  \item{ctmatrix}{
matrix representing a square contingency table between a comparison variable (rows) and a reference variable (columns)
}
}
\value{
a numeric vector containing the exchange difference between the comparison variable and the reference variable at the category level. Output values are given in the same units as \code{ctmatrix}
}
\references{
Pontius Jr., R.G., Santacruz, A. 2014. \emph{Quantity, exchange and shift components of difference in a square contingency table}. International Journal of Remote Sensing 35 (21), 7543-7554.
}
\seealso{
\code{\link{overallQtyD}}
}
\examples{
comp <- raster(system.file("external/comparison.rst", package="diffeR"))
ref <- raster(system.file("external/reference.rst", package="diffeR"))
ctmatCompRef <- crosstabm(comp, ref)
exchangeDj(ctmatCompRef)
}

\keyword{ spatial }
