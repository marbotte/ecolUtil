\name{mat2dbTab}
\alias{mat2dbTab}
\title{
Formatting an ecological site x species matrix into a database table
}
\description{
}
\usage{
mat2dbTab(mat,checklist=F)
}
\arguments{
  \item{mat}{Ecological site x species matrix. Sites (locations) should be rows, species columns}
  \item{checklist}{TRUE if simple checklist giving the occurrences of the variables}
}
\value{
A data frame with the colums "SU" (sampling units), "sp" (species) and "ab" abundance.
If checklist is TRUE, the column "ab" wont be created
}
\author{Bottin, Marius}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dbTab2mat}}
}
\examples{
matSp<-matrix(sample(c(0,1),100,replace=T,prob=c(.7,.3)),nrow=10,ncol=10,dimnames=list(paste("site",1:10,sep=""),paste("sp",1:10,sep="")))
mat2dbTab(matSp)
}
\keyword{ manip }
