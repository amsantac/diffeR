\name{overallComponentsPlot}
\alias{overallComponentsPlot}

\title{
Overall Components plot 
}
\description{
create the Overall Components plot from the comparison between a comparison map (or map at time \emph{t}) and a reference map (or map at time \emph{t}+1)
}
\usage{
overallComponentsPlot(comp, ref)
}

\arguments{
  \item{comp}{
object of class RasterLayer corresponding to a comparison map (or map at time \emph{t})
}
  \item{ref}{
object of class RasterLayer corresponding to a reference map (or map at time \emph{t}+1)
}
}
\value{
a stacked barplot showing the quantity, exchange and shift components of difference between the comparison map (or map at time \emph{t}) and the reference map (or map at time \emph{t}+1)
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
overallComponentsPlot(comp, ref)
}

\keyword{ spatial }
