\name{reduceTable}
\alias{reduceTable}
\title{Delete recursively rows and columns of a table till row and column sums have a minimum and/or a maximum value}
\description{
The function is designed to delete species and sampling units (rows) of a species occurrence table to prepare them for multivariate analysis.
Species that are too scarse, or too common may add noises in the analyses.
All the same, sampling units with too much differences in their number of species may add noise.
When deleting a species, we may reach the threshold for deleting a sampling unit as well, and the opposite, so we use a recursive process here.
}
\usage{
reduceTable(tab, sumRowMin = 5, sumRowMax = NA, sumColMin = 3, sumColMax = NA, uniq = T, trace = T)
}
\arguments{
  \item{tab}{
					Species occurrence (or abundance matrix) Note that if the matrix contains abundances, the algorithm will calculate the sum of columns and rows anyway, the thresholds will not be in terms of occurrences)
}
  \item{sumRowMin}{Values for the row sums under which we suppress the rows, if NA no threshold is applied
}
  \item{sumRowMax}{Maximal values for the row sums. if NA no threshold is applied}
  \item{sumColMin}{Minimal values for the column sums. If NA no threshold is applied
}
  \item{sumColMax}{Maximal values for the column sums, if NA, no threshold is applied}
  \item{uniq}{If TRUE, the rows (sampling units) that show the exact same composition than the others are deleted. This is a condition for applying the vegan NMDS for example.
}
  \item{trace}{If True the algorithm will give informations while running (number of pass), and successive dimensions of the table.
}
}
\details{
The maximum and minimum values are tested with pure inferior and pure superior operators. If the value of a row or col sum is equal to the threshold, the row or col is kept in the final table.
}
\value{
				Matrix of the same type than tab.
}
\author{
				Bottin, Marius
}
\examples{
}
\keyword{ manip }
