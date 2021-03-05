\name{ClusterInterDistances}
\alias{ClusterInterDistances}
\alias{InterClusterDistances}

\title{
Computes Inter-Cluster Distances
}
\description{
Computes inter-cluster distances which are the distance between each cluster and all other clusters
}
\usage{
ClusterInterDistances(FullDistanceMatrix, Cls,

Names,PlotIt=FALSE)
}

\arguments{
  \item{FullDistanceMatrix}{
  [1:n,1:n] symmetric distance matrix
}
  \item{Cls}{
[1:n] numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
  \item{Names}{
  Optional [1:k] character vector naming k classes
}
  \item{PlotIt}{
  Optional, Plots if TRUE
}
}
\details{
Cluster distances are given back as a matrix, one column per cluster and the vector of the full distance matrix without the diagonal elements and the upper half of the symmetric matrix. Details and definitons can be found in [Thrun, 2021].
}
\value{
Matrix [1:m,1:(k+1)] of k clusters, each columns consists of the distances between a cluster and all other clusters, filled up with NaN at the end to be of the same lenght as the vector of the upper triangle of the complete distance matrix.
}
\author{
Michael Thrun
}
\references{
[Thrun, 2021]  Thrun, M. C.: The Exploitation of Distance Distributions for Clustering, International Journal of Computational Intelligence and Applications, Vol. accepted, 2021.
}
\seealso{
\code{\link[DataVisualizations]{MDplot}}

\code{ClusterDistances}
}
\examples{
data(Hepta)
Distance=as.matrix(dist(Hepta$Data))

interdists=ClusterInterDistances(Distance,Hepta$Cls)
}

\concept{inter cluster}
\keyword{distances}
\keyword{intercluster}