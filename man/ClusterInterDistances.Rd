\name{ClusterInterDistances}
\alias{ClusterInterDistances}
\alias{InterClusterDistances}

\title{
Computes Inter-Cluster Distances
}
\description{
Computes the distances between each cluster and all other clusters.
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
A matrix with k + 1 columns. The first column contains all pairwise distances; each remaining column contains the distances between one cluster and all other clusters. Shorter columns are padded with \code{NaN} to match the length of the upper triangle of the complete distance matrix.
}
\author{
Michael Thrun
}
\references{
[Thrun, 2021]  Thrun, M. C.: The Exploitation of Distance Distributions for Clustering, International Journal of Computational Intelligence and Applications, Vol. 20(3), pp. 2150016, DOI: \doi{10.1142/S1469026821500164}, 2021.
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