\name{pnCentrality}
\alias{pnCentrality}
\alias{pnCentrality_val}
\title{
				Calculate the PN centrality of a non-valued or valued network.
}
\description{
				The PN centrality is a measure of centrality that takes into account the negative ties in a network
}
\usage{
pnCentrality(posit, negat)
pnCentrality_val(posit,negat,fac_beta=max(c(posit,negat))*2)
}
\arguments{
  \item{posit}{
The adjacency matrix representing the positive relationships in a network.
}
  \item{negat}{
The adjacency matrix representing the negative relationships in a network. Note that the relationships have to be positive (1) in this matrix even though they represents the negative relationships
}
}
\details{
				The formula given for the PN centrality is \deqn{PN=(I-\frac{1}{2n-2}A)^{-1}\boldsymbol{1}}{PN=(I-\frac{1}{2n-2}A)^{-1}*1}}

Where

\deqn{A=P-2N}{A=P-2*N}

P is the positive tie matrix

N is the negative tie matrix

n is the number of vertices

I is the identity matrix


The formula we derivate from it for valued PN centrality is \deqn{PN=(I-\frac{1}{2fn-2f}A)^{-1}\boldsymbol{1}}{PN=(I-\frac{1}{fn-f}A)^{-1}*1}}

Where f is the maximal value in the network (we estimate here that the positive maximal value is equivalent to the absolute value of the negative 
}
\value{
				Returns a vector of PN centrality values (the length of the vector corresponds to the number of vertices)
}
\references{
@article{Everett2014NetworksCN,
		title={Networks containing negative ties},
		author={Martin G. Everett and Stephen P. Borgatti},
		journal={Social Networks},
		year={2014},
		volume={38},
		pages={111-120}
}
}

\author{
				Bottin, Marius
}
\note{
The function for the valued networks is experimental and has not been tested sufficiently with classical example. Use at tour own risk!
}


\seealso{
}
\examples{
data(mixedTie)
P<-N<-mixedTie
P[P<=0]<-0
N[N>=0]<-0
N<-abs(N)
pnCentrality(P,N)
}
\keyword{ graphs }
