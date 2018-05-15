\name{mat2dbTab}
\alias{mat2dbTab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Formatting an ecological site x species matrix into a database table
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mat2dbTab(mat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{Ecological site x species matrix. Sites (locations) should be rows, species columns}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame with the colums "SU" (sampling units), "sp" (species) and "ab" abundance.

}
\author{Bottin, Marius}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dbTab2mat}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
matSp<-matrix(sample(c(0,1),100,replace=T,prob=c(.7,.3)),nrow=10,ncol=10,dimnames=list(paste("site",1:10,sep=""),paste("sp",1:10,sep="")))
mat2dbTab(matSp)
## The function is currently defined as
function (mat) 
{
    W <- which(mat > 0, arr.ind <- T)
    dbTab <- data.frame(SU = rownames(mat)[W[, "row"]], sp = colnames(mat)[W[, 
        "col"]], ab = mat[W])
    dbTab <- dbTab[order(dbTab$SU, dbTab$sp), ]
    return(dbTab)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
