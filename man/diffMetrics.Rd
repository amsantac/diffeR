\name{diffMetrics}
\alias{differenceMetrics}
\alias{exchangeDij}
\alias{exchangeDj}
\alias{overallAllocD}
\alias{overallDiff}
\alias{overallDiffCatj}
\alias{overallExchangeD}
\alias{overallQtyD}
\alias{overallShiftD}
\alias{quantityDj}
\alias{shiftDj}
\alias{omissionj}
\alias{comissionj}
\alias{agreementj}

\title{
Overall difference metrics and difference metrics at the category level or between pairs of categories
}
\description{
Overall difference metrics (overallAllocD, overallDiff, overallDiffCatj, overallExchangeD, overallQtyD, overallShiftD) and difference metrics at the category level (exchangeDj, quantityDj, shiftDj, omissionj, commisionj, agreementj) or between pairs of categories (exchangeDij) calculated using a square contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1) 
}
\usage{
exchangeDij(ctmatrix)
exchangeDj(ctmatrix)
overallAllocD(ctmatrix)
overallDiff(ctmatrix)
overallDiffCatj(ctmatrix)
overallExchangeD(ctmatrix)
overallQtyD(ctmatrix)
overallShiftD(ctmatrix)
quantityDj(ctmatrix)
shiftDj(ctmatrix)
omissionj(ctmatrix)
comissionj(ctmatrix)
agreementj(ctmatrix)
}

\arguments{
  \item{ctmatrix}{
matrix representing a square contingency table between a comparison variable (rows) and a reference variable (columns)
}
}

\value{
\code{exchangeDij} returns a matrix containing exchange occurring between pairs of categories from the comparison variable and the reference variable. Exchange is shown in the lower triangle of the output matrix. 

\code{exchangeDj} returns a numeric vector containing the exchange difference between the comparison variable and the reference variable at the category level.

\code{overallAllocD} returns the overall allocation difference between the comparison variable and the reference variable. 

\code{overallDiff} returns the overall difference between the comparison variable and the reference variable.

\code{overallDiffCatj} returns a numeric vector containing overall difference between the comparison variable and the reference variable at the category level. 

\code{overallExchangeD} returns the overall exchange difference between the comparison variable and the reference variable.

\code{overallQtyD} returns the overall quantity difference between the comparison variable and the reference variable. 

\code{overallShiftD} returns the overall shift difference between the comparison variable and the reference variable.

\code{quantityDj} returns a numeric vector containing the quantity difference between the comparison variable and the reference variable at the category level. 

\code{shiftDj} returns a numeric vector containing the shift difference between the comparison variable and the reference variable at the category level.

\code{omissionj} returns a numeric vector containing the omission disagreement between the comparison variable and the reference variable at the category level.

\code{comissionj} returns a numeric vector containing the comission disagreement between the comparison variable and the reference variable at the category level.

\code{agreementj} returns a numeric vector containing the agreement between the comparison variable and the reference variable at the category level.

For all the functions above, the output values are given in the same units as \code{ctmatrix}.
}

\details{
\code{exchangeDij} calculates the exchange matrix between pairs of categories from a square contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). Exchange consists of a transition from category \emph{i} to category \emph{j} in some observations and a transition from category \emph{j} to category \emph{i} in an identical number of other observations. 

\code{exchangeDj} calculates exchange difference at the category level from a square contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). Exchange consists of a transition from category \emph{i} to category \emph{j} in some observations and a transition from category \emph{j} to category \emph{i} in an identical number of other observations.

\code{overallAllocD} calculates overall allocation difference from a square contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). Allocation difference is defined as the amount of difference between a reference variable and a comparison variable that is due to the less than maximum match in the spatial allocation of the categories, given the proportions of the categories in the reference and comparison variables. Allocation difference is equivalent to the addition of the exchange and shift components of difference (i.e., allocation difference can be disaggregated into exchange and shift components).

\code{overallDiff} calculates overall difference from a square contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). Overall difference is equivalent to the addition of the quantity and allocation components of difference (i.e., overall difference can be disaggregated into quantity and allocation components).

\code{overallDiffCatj} calculates overall difference at the category level from a square contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). Overall difference is equivalent to the addition of the quantity and allocation components of difference (i.e., overall difference can be disaggregated into quantity and allocation components).

\code{overallExchangeD} calculates overall exchange difference from a square contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1).
Exchange consists of a transition from category \emph{i} to category \emph{j} in some observations and a transition from category \emph{j} to category \emph{i} in an identical number of other observations.

\code{overallQtyD} calculates overall quantity difference from a square contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). Quantity difference is defined as the amount of difference between the reference variable and a comparison variable that is due to the less than maximum match in the proportions of the categories.

\code{overallShiftD} calculates overall shift difference from a square contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). Shift refers to the difference remaining after subtracting quantity difference and exchange from the overall difference.

\code{quantityDj} calculates quantity difference at the category level from a square contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). Quantity difference is defined as the amount of difference between the reference variable and a comparison variable that is due to the less than maximum match in the proportions of the categories.

\code{shiftDj} calculates shift difference at the category level from a contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). Shift refers to the difference remaining after subtracting quantity difference and exchange from the overall difference.

\code{omissionj} calculates omission error at the category level from a contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). Omission error refers to observations in the reference variable that were classified in a category other than their true or known category, in other words, observations of a known category that were excluded from that category due to misclassification. 

\code{comissionj} calculates comission error at the category level from a contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). Commission error refers to observations that were incorrectly classified and do not belong in the category in which they were assigned according to the classification. 

\code{agreementj} calculates agreement at the category level from a contingency table derived from the crosstabulation between a comparison variable (or variable at time \emph{t}), and a reference variable (or variable at time \emph{t}+1). 
}

\references{
Pontius Jr., R.G., Millones, M. 2011. \emph{Death to Kappa: birth of quantity disagreement and allocation disagreement for accuracy assessment}. International Journal of Remote Sensing 32 (15), 4407-4429.

Pontius Jr., R.G., Santacruz, A. 2014. \emph{Quantity, exchange and shift components of difference in a square contingency table}. International Journal of Remote Sensing 35 (21), 7543-7554.
}
\seealso{
\code{\link{diffTablej}}
}
\examples{
comp <- raster(system.file("external/comparison.rst", package="diffeR"))
ref <- raster(system.file("external/reference.rst", package="diffeR"))
ctmatCompRef <- crosstabm(comp, ref)

exchangeDij(ctmatCompRef)
exchangeDj(ctmatCompRef)
overallAllocD(ctmatCompRef)
overallDiff(ctmatCompRef)
overallDiffCatj(ctmatCompRef)
overallExchangeD(ctmatCompRef)
overallQtyD(ctmatCompRef)
overallShiftD(ctmatCompRef)
quantityDj(ctmatCompRef)
shiftDj(ctmatCompRef)
omissionj(ctmatCompRef)
comissionj(ctmatCompRef)
agreementj(ctmatCompRef)
}

\keyword{ internal }
