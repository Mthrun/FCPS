\name{ClusterCount}
\alias{ClusterCount}
\title{ClusterCount}
\description{
Calculates cluster counts and percentages.
}
\usage{
ClusterCount(Cls,Ordered=TRUE,NonFinite=9999)
}
\arguments{
  \item{Cls}{1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.}
    \item{Ordered}{
    Optional logical scalar. If \code{TRUE}, the output is ordered increasingly by the labels in \code{UniqueClusters}.
    }
   \item{NonFinite}{
    Optional scalar used to replace non-finite values in \code{Cls}.}

}

\value{
\item{UniqueClusters}{A numerical vector containing the k unique cluster labels in \code{Cls}.}
\item{CountPerCluster}{A named vector of length k containing the number of observations in each cluster. Its names are \code{UniqueClusters}.}
\item{NumberOfClusters}{The number of clusters, k.}
\item{ClusterPercentages}{A numerical vector of length k containing the percentage of observations in each cluster.}
}
\details{
When \code{Ordered = FALSE}, output order is determined by the first occurrence of each cluster label in \code{Cls}.

Non-numeric vectors are supported. In that case, \code{Cls} is converted with \code{as.character()}, a warning is issued, and the statistics are computed.
}
\author{Michael Thrun}

\examples{
data('Hepta')
Cls=Hepta$Cls
ClusterCount(Cls)
}


\keyword{ClusterCount}
\concept{Cluster Count}