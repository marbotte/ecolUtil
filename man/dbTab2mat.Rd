\name{dbTab2mat}
\alias{dbTab2mat}
\title{Transform a checklist of species into a matrix of presences or abundances of species}
\description{The function }
\usage{
dbTab2mat(dbTab,col_samplingUnits="SU",col_species="sp",col_content="abundance",empty=NA,checklist=F)
}
\arguments{
  \item{dbTab}{data.frame containing the data}
  \item{col_samplingUnits}{name of the column of dbTab which contains the locations, sites or sampling units}
  \item{col_species}{name of the column of dbTab which contains the species names}
  \item{col_content}{name of the column of dbTab which contains the presence, abundance or any content that we want to fill the matrix}
  \item{empty}{Value used to fill the matrix when the couple Sampling unit / species is not given in dbTab (absences of the species). If NA, the value is deduced from the mode of the col_content column}
  \item{checklist}{True when there is actually no col_content, and that dbTab is only a checklist of species occurrences}
}
\value{
A table sampling units/species with presences or abundances of the species. Note that if checklist is TRUE, then the result will be a logical matrix, that takes less memory space. Note that mutiplicating
}
\author{Bottin, Marius}
\seealso{
  \code{\link{mat2dbTab}}
}
\examples{
(dbTab<-data.frame(SU=rep(paste("site",1:10,sep=""),10),sp=rep(paste("sp",1:10,sep=""),each=10),abundance=sample(1:5,100,prob=c(10,7,3,2,1),replace=T))[sample(1:100,30),])
dbTab2mat(dbTab)
}
\keyword{ manip }

