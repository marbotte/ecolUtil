\name{pnCentrality}
\alias{pnCentrality}
\title{
				Calculate the PN centrality of a non-valued network.
}
\description{
				The PN centrality is a measure of centrality that takes into account the negative ties in a network
}
\usage{
pnCentrality(posit, negat)
}
%- maybe also 'usage' for other objects documented here.
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(mixedTie)
P<-N<-mixedTie
P[P<=0]<-0
N[N>=0]<-0
N<-abs(N)
pnCentrality(P,N)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphs }% use one of  RShowDoc("KEYWORDS")
