\name{is.outlier}
\alias{is.outlier}
\title{
				Are the values of one vector outliers.
}
\description{
				Give TRUE for the values of the vectors that are superior to Median+factor(InterquartileDifference)
}

\usage{
is.outlier(x, fac = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector in which we search the outliers
}
  \item{fac}{
					Factor multiplicative of the interquartile distance for finding the thresholds of outliers. Default value of 3
}
}
\details{
				The formula used is the following
				\deqn{x>M+(fac\times{IQ})}
				OR
				\deqn{x>M-(fac\times{IQ})}
				Where M is the median of x and IQ the interquartile distance.
}
\value{
				A logical vectors that is TRUE when the corresponding value of x is considered an outlier.
}
\author{
				Bottin, Marius
}
\examples{
(A<-rnorm(100))
is.outlier(A)
}
\keyword{ distribution }% use one of  RShowDoc("KEYWORDS")

