\name{EntropyOfDataField}
\alias{EntropyOfDataField}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Entropy Of a Data Field [Wang et al., 2011].
}
\description{
Calculates the Potential Entropy Of a Data Field for a given ranges of impact factors sigma
}
\usage{
EntropyOfDataField(Data, 

sigmarange = c(0.01, 0.1, 0.5, 1, 2, 5, 8, 10, 100)

, PlotIt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n,1:d] data matrix
}
  \item{sigmarange}{
Numeric vector [1:s] of relevant sigmas
}
  \item{PlotIt}{
FALSE: disable plot, TRUE: Plot with upper boundary of H after [Wang et al., 2011].
}
}
\details{
In theory there should be a curve with a clear minimum of Entropy [Wang et al.,2011]. Then the choice for the impact factor sigma is the minimum of the entropy to define the correct data field. It follows, that the influence radius is 3/sqrt(2)*sigma (3B rule of gaussian distribution) for clustering algorithms like density peak clustering [Wang et al.,2011].
}
\value{
[1:s] named vector of the Entropy of data field. The names are the impact factor sigma.
}
\references{
[Wang et al., 2015] Wang, S., Wang, D., Li, C., & Li, Y.: Comment on" Clustering by fast search and find of density peaks", arXiv preprint arXiv:1501.04267, 2015.

[Wang et al., 2011]  Wang, S., Gan, W., Li, D., & Li, D.: Data field for hierarchical clustering, International Journal of Data Warehousing and Mining (IJDWM), Vol. 7(4), pp. 43-63. 2011.


}
\author{
Michael Thrun
}


\examples{
data(Hepta)
H=EntropyOfDataField(Hepta$Data,PlotIt=FALSE)
Sigmamin=names(H)[which.min(H)]
Dc=3/sqrt(2)*as.numeric(names(H)[which.min(H)])
}

\concept{data field}
\keyword{entropy}
\concept{data entropy}