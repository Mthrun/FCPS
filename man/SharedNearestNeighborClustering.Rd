\name{SharedNearestNeighborClustering}
\alias{SharedNearestNeighborClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SNN clustering 
}
\description{
Shared Nearest Neighbor Clustering of [Ertoz et al., 2003].
}
\usage{
SharedNearestNeighborClustering(Data,Knn=7,

Radius,minPts,PlotIt=FALSE,

UpperLimitRadius,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}
  \item{Knn}{
Number of neighbors to consider to calculate the shared nearest neighbors.
}
 \item{Radius}{
 Eps [Ester et al., 1996, p. 227] neighborhood in the R-ball graph/unit disk graph), size of the epsilon neighborhood.
 If NULL, automatic estimation is done using insights of [Ultsch, 2005].
}
  \item{minPts}{
 Number of minimum points in the eps region (for core points). 
 In principle minimum number of points in the unit disk, if the unit disk is within the cluster (core) [Ester et al., 1996, p. 228].
 if NULL, its 2.5 percent of points.
}
\item{PlotIt}{Default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{UpperLimitRadius}{Limit for radius search, experimental}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}

}
\details{
..
}
\value{
List of
\item{Cls}{[1:n]  numerical vector defining the clustering; this classification is the main output of the algorithm. Points which cannot be assigned to a cluster will be reported as members of the noise cluster with 0.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Ertoz et al., 2003]   Levent Ertoz, Michael Steinbach, Vipin Kumar: Finding Clusters of Different Sizes, Shapes, and Densities in Noisy, High Dimensional Data, SIAM International Conference on Data Mining, 47-59, 2003.
}
\author{
Michael Thrun
}

\seealso{
\code{\link[dbscan]{sNNclust}}
}
\examples{
data('Hepta')
out=SharedNearestNeighborClustering(
Hepta$Data,Radius=NULL,minPts=NULL,PlotIt = FALSE)
}

\keyword{snn}% use one of  RShowDoc("KEYWORDS")
\concept{SharedNearest Neighbor Clustering}% __ONLY ONE__ keyword per line
