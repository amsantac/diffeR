\name{overallSourcesPlot}
\alias{overallSourcesPlot}

\title{
Overall Sources plot 
}
\description{
create the Overall Sources plot from the comparison between a comparison map (or map at time \emph{t}) and a reference map (or map at time \emph{t}+1)
}
\usage{
overallSourcesPlot(comp = NULL, ref = NULL, ctmatrix = NULL, units = NULL, population = NULL)
}

\arguments{
  \item{comp}{
object of class RasterLayer corresponding to a comparison map (or map at time \emph{t})
}
  \item{ref}{
object of class RasterLayer corresponding to a reference map (or map at time \emph{t}+1)
}
  \item{ctmatrix}{
matrix representing a square contingency table between a comparison variable (rows) and a reference variable (columns)
}
  \item{units}{
character string indicating units of \code{ctmatrix}.
}
  \item{population}{
an \emph{n} x 2 matrix provided to correct the sample count to population count in the square contingency table. See Details below}
}

\details{
The first column of \code{population} must contain integer identifiers of each category, corresponding to the categories in the comparison and reference variables. The second column corresponds to the population totals for each category.  
}

\value{
a stacked barplot showing the omission and comission components of difference between the comparison map (or map at time \emph{t}) and the reference map (or map at time \emph{t}+1)
}
\references{
Pontius Jr., R.G., Millones, M. 2011. \emph{Death to Kappa: birth of quantity disagreement and allocation disagreement for accuracy assessment}. International Journal of Remote Sensing 32 (15), 4407-4429.
}
\seealso{
\code{\link{diffTablej}}
}
\examples{
comp <- raster(system.file("external/comparison.rst", package="diffeR"))
ref <- raster(system.file("external/reference.rst", package="diffeR"))
overallSourcesPlot(comp, ref)

ctmat <- crosstabm(comp, ref)
overallComponentsPlot(ctmatrix = ctmat)
}

\keyword{ spatial }
