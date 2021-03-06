\name{HierarchicalClustering}
\alias{HierarchicalClustering}

\title{
Hierarchical Clustering
}
\description{
Wrapper for various agglomerative hierarchical clustering algorithms.
}
\usage{
HierarchicalClustering(DataOrDistances,ClusterNo,Type='SingleL',Fast=TRUE,Data,\dots)
}

\arguments{
\item{DataOrDistances}{
Either nonsymmetric [1:n,1:d] numerical matrix of a dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.

or

symmetric [1:n,1:n] distance matrix, e.g. \code{as.matrix(dist(Data,method))}

}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{Type}{Method of cluster analysis: "Ward", "SingleL", "CompleteL", "AverageL" (UPGMA), "WPGMA" (mcquitty), "MedianL" (WPGMC), "CentroidL" (UPGMC), "Minimax", "MinEnergy", "Gini","HDBSCAN", or "Sparse"}
\item{Fast}{If TRUE and fastcluster installed, then a faster implementation of the methods above can be used except for "Minimax", "MinEnergy", "Gini" or "HDBSCAN"}

\item{Data}{[1:n,1:d] data matrix in the case that \code{DataOrDistances} is missing and partial matching does not work.}

  \item{\dots}{
Further arguments passed on to either \code{\link{HierarchicalClusterData}}, \code{\link{HierarchicalClusterDists}}, \code{\link{MinimalEnergyClustering}} or \code{\link{GenieClustering}} (for "Gini"), \code{\link{HierarchicalDBSCAN}} (for HDBSCAN) or \code{\link{SparseClustering}} (for Sparse).
}
}
\details{
Please see \code{\link{HierarchicalClusterData}} and \code{\link{HierarchicalClusterDists}} or the other functions listed above.

It should be noted that in case of "HDBSCAN" the number of clusters is manually selected by \code{cutree} to have the same convention as the other algorithms. Usually, "HDBSCAN" selects the number of clusters automatically.
}
\value{
List of
\item{Cls}{If, ClusterNo>0: [1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering. Otherwise for ClusterNo=0: NULL}
\item{Dendrogram}{Dendrogram of hierarchical clustering algorithm}
\item{Object}{Ultrametric tree of hierarchical clustering algorithm}
}

\author{
Michael Thrun
}

\seealso{
\code{\link{HierarchicalClusterData}}

\code{\link{HierarchicalClusterDists}},

\code{\link{MinimalEnergyClustering}}.
}
 \examples{
data('Hepta')
out=HierarchicalClustering(Hepta$Data,ClusterNo=7)
}

\keyword{HierarchicalClustering}
\keyword{Hierarchical}
\keyword{Clustering}
\concept{Hierarchical Clustering}