\name{overallSourcesPlot}
\alias{overallSourcesPlot}

\title{
Overall Sources plot 
}
\description{
If comparison and reference (raster) maps are provided, this function creates the Overall Sources plot from the comparison between the comparison map (or map at time \emph{t}) and the reference map (or map at time \emph{t}+1). If a square contingency table (matrix) is provided instead of raster maps, then this function creates the Overall Sources plot from the comparison between the comparison variable (or variable at time \emph{t}) and the reference variable (or variable at time \emph{t}+1).
}
\usage{
overallSourcesPlot(comp = NULL, ref = NULL, ctmatrix = NULL, analysis = "error", 
                    units = NULL, population = NULL, fontSize = NULL, colorValues = NULL)
}

\arguments{
  \item{comp}{
object of class RasterLayer corresponding to a comparison map (or map at time \emph{t}). See Details below
}
  \item{ref}{
object of class RasterLayer corresponding to a reference map (or map at time \emph{t}+1). See Details below
}
  \item{ctmatrix}{
matrix representing a square contingency table between a comparison variable (rows) and a reference variable (columns). See Details below
}
  \item{analysis}{
character string indicating type of analysis, either "error" (default) or "change"
}
  \item{units}{
character string indicating units of \code{ctmatrix}
}
  \item{population}{
an \emph{n} x 2 matrix provided to correct the sample count to population count in the square contingency table. See Details below
}
  \item{fontSize}{
text size (in pts)
}
  \item{colorValues}{
a set of aesthetic values to map data values to. If this is a named vector, then the values will be matched based on the names. If unnamed, values will be matched in order (usually alphabetical) with the limits of the scale. Any data values that don't match will be given \code{na.value}.
}
}

\details{
Users may enter as input either a square contingency table (\code{ctmatrix}) or a comparison and a reference raster maps (\code{comp} and \code{ref}, respectively).

The first column of \code{population} must contain integer identifiers of each category, corresponding to the categories in the comparison and reference variables. The second column corresponds to the population totals for each category.  
}

\value{
a stacked barplot showing the omission and comission components of difference between the comparison map/variable (or map/variable at time \emph{t}) and the reference map/variable (or map/variable at time \emph{t}+1)
}
\references{
Pontius Jr., R.G., Millones, M. 2011. \emph{Death to Kappa: birth of quantity disagreement and allocation disagreement for accuracy assessment}. International Journal of Remote Sensing 32 (15), 4407-4429.
}
\seealso{
\code{\link{differenceMetrics}}
}
\examples{
comp <- raster(system.file("external/comparison.rst", package="diffeR"))
ref <- raster(system.file("external/reference.rst", package="diffeR"))
overallSourcesPlot(comp, ref)

ctmat <- crosstabm(comp, ref)
overallSourcesPlot(ctmatrix = ctmat, units = "pixels")
}

\keyword{ spatial }
