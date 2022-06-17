\name{ClusterCount}
\alias{ClusterCount}
\title{ClusterCount}
\description{
Calulates statistics for clustering in each group of the data points
}
\usage{
ClusterCount(Cls,Ordered=TRUE,NonFinite=9999)
}
\arguments{
  \item{Cls}{1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.}
    \item{Ordered}{
    Optional, boolean, if TRUE: the ouput is ordered increasingly by cluster labels in \code{UniqueClusters}.
    }
   \item{NonFinite}{
    Optional, If non finite values are given in the numerical vector, they are set to the scalar value defined here}
    
}

\value{
\item{UniqueClusters}{[1:k] numerical vector of the k unique clusters in Cls}
\item{CountPerCluster}{Named vector [1:k] with the number of data points in the corresponding unique clusters. Names are the \code{UniqueClusters}}
\item{NumberOfClusters}{The number of clusters k}
\item{ClusterPercentages}{[1:k] numerical vector of the percentages of datapoints belonging to a cluster for each cluster}
}
\details{The ordering of the output is defined by the first occurence of every cluster label in \code{Cls} in the  setting of \code{Ordered=FALSE}. }
\author{Michael Thrun}

\examples{
data('Hepta')
Cls=Hepta$Cls
ClusterCount(Cls)
}


\keyword{ClusterCount}
\concept{Cluster Count}