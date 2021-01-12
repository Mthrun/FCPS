\name{ClusterEqualWeighting}
\alias{ClusterEqualWeighting}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ClusterEqualWeighting
}
\description{
Weights clusters equally
}
\usage{
ClusterEqualWeighting(Cls, Data, MinClusterSize)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Cls}{
1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
  \item{Data}{
Optional, [1:n,1:d] matrix of dataset consisting of n cases of d-dimensional data points. Every case has d attributes, variables or features.
}
  \item{MinClusterSize}{
Optional, scalar defining the number of cases m that each cluster should have
}
}
\details{
Balance clusters such that their sizes are the same by subsampling the larger cluster. If \code{MinClusterSize} is missing the number of cases per cluster is set to the smallest cluster size.
}
\value{
List of
\item{BalancedCls }{Vector of Cls such that all clusters have the same sizes spezified by \code{MinClusterSize}}
\item{BalancedInd }{index such that BalancedCls = Cls[BalancedInd]}
\item{BalancedData }{NULL if missing, otherwise, Data[BalancedInd,]}
}

\author{
Alfred Ultsch (matlab), reimplemented by Michael Thrun 
}


\examples{
data(Hepta)
ClusterEqualWeighting(Hepta$Cls,Hepta$Data,5)
}

\keyword{ClusterEqualWeighting}
\concept{Equal Weighting}

