\name{sample2pop}
\alias{sample2pop}

\title{
corrects sample counts to population counts in a square contingency table
}
\description{
Converts sample count to population count in the square contingency table, assuming a stratified random sampling
}
\usage{
sample2pop(ctmatrix, population)
}

\arguments{
  \item{ctmatrix}{
matrix representing a sampling-derived square contingency table between a comparison variable (rows) and a reference variable (columns)
}
  \item{population}{
an \emph{n} x 2 matrix provided to correct the sample count to population count in the square contingency table. See Details below}
}
\details{
The first column of \code{population} must contain integer identifiers of each category, corresponding to the categories in the comparison and reference variables. The second column corresponds to the population totals for each category.  
}
\value{
matrix representing a population-adjusted square contingency table for the crosstabulation between a comparison variable (rows) and a reference variable (columns). Output values are given in the same units as \code{ctmatrix}
}
\seealso{
\code{\link{crosstabm}}
}
\examples{
comp <- raster(system.file("external/comparison.rst", package="diffeR"))
ref <- raster(system.file("external/reference.rst", package="diffeR"))

# Sample square contingency table
(ctmatCompRef <- crosstabm(comp, ref))

# Population-adjusted square contingency table
(population <- matrix(c(1,2,3,2000,4000,6000), ncol=2))
sample2pop(ctmatCompRef, population = population)

# The square contingency table can also be adjusted directly using the crosstabm function
crosstabm(comp, ref, population = population)
}

\keyword{ spatial }
