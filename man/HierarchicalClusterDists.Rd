\name{HierarchicalClusterDists}

\alias{HierarchicalClusterDists}

\title{
Hierarchical Clustering with Distances
}
\usage{
HierarchicalClusterDists(pDist,ClusterNo=0,Type="ward.D2",

ColorTreshold=0,Fast=FALSE,\dots)
}

\description{
Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it. Uses stats package function 'hclust'.
}

\arguments{
\item{pDist}{Distances as either matrix [1:n,1:n] or dist object}
\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{Type}{Method of cluster analysis: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".}
\item{ColorTreshold}{Draws cutline w.r.t. dendogram y-axis (height), height of line as scalar should be given}
\item{Fast}{If TRUE and fastcluster installed, then a faster implementation of the methods above can be used}
\item{\dots}{In case of plotting further argument for \code{plot}, see \code{\link{as.dendrogram}}
}
}

\value{
List of
\item{Cls}{If, ClusterNo>0: [1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering. Otherwise for ClusterNo=0: NULL}
\item{Dendrogram}{Dendrogram of hierarchical clustering algorithm}
\item{Object}{Ultrametric tree of hierarchical clustering algorithm}
}
\author{Michael Thrun}
 \examples{
data('Hepta')
#out=HierarchicalClusterDists(as.matrix(dist(Hepta$Data)),ClusterNo=7)
}
\keyword{Hierarchical}
\keyword{Clustering}
\keyword{Distances}