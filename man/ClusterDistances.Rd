\name{ClusterDistances}
\alias{ClusterIntraDistances}
\alias{IntraClusterDistances}
\alias{ClusterDistances}

\title{
ClusterDistances
}
\description{
Computes the within-cluster distances for each cluster.
}
\usage{
ClusterDistances(FullDistanceMatrix, Cls,

Names, PlotIt = FALSE)

ClusterIntraDistances(FullDistanceMatrix, Cls,

Names, PlotIt = FALSE)
}
\arguments{
  \item{FullDistanceMatrix}{
  [1:n,1:n] symmetric distance matrix
}
  \item{Cls}{
  [1:n] numerical vector of k classes
}
  \item{Names}{
  Optional [1:k] character vector naming k classes
}
  \item{PlotIt}{
  Optional, Plots if TRUE
}
}
\details{
Returns a matrix with one column per cluster and one column containing all pairwise distances from the upper triangle of the complete distance matrix.
Details and definitions can be found in [Thrun, 2021].
}
\value{
A matrix with k + 1 columns. The first column contains all pairwise distances; each remaining column contains the within-cluster distances for one cluster. Shorter columns are padded with \code{NaN}; a singleton cluster therefore has \code{NaN} as its within-cluster distance.
}
\author{
Michael Thrun
}
\references{
[Thrun, 2021]  Thrun, M. C.: The Exploitation of Distance Distributions for Clustering, International Journal of Computational Intelligence and Applications, Vol. 20(3), pp. 2150016, DOI: \doi{10.1142/S1469026821500164}, 2021.
}
\seealso{
\code{\link[DataVisualizations]{MDplot}}

\code{\link{ClusterInterDistances}}
}
\examples{
data(Hepta)
Distance=as.matrix(dist(Hepta$Data))

interdists=ClusterDistances(Distance,Hepta$Cls)
}

\concept{intra cluster}
\keyword{distances}
\keyword{intracluster}