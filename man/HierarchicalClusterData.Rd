\name{HierarchicalClusterData}
\alias{HierarchicalClusterData}
\alias{HierarchicalCluster}

\title{
Internal function of Hierarchical Clusterering of Data
}
\usage{
HierarchicalClusterData(Data,ClusterNo=0,

Type="ward.D2",DistanceMethod="euclidean",

ColorTreshold=0,Fast=FALSE,Cls=NULL,\dots)
}

\description{
Please use \code{\link{HierarchicalClustering}}. Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it. Uses stats package function 'hclust'.
}
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}
\item{ClusterNo}{A number k which defines k different clusters to be build by the algorithm.}
\item{Type}{Methode der Clusterung: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".}
\item{DistanceMethod}{see  \code{\link[parallelDist]{parDist}}, for example 'euclidean','mahalanobis','manhatten' (cityblock),'fJaccard','binary', 'canberra', 'maximum'. Any unambiguous substring can be given.}
\item{ColorTreshold}{Draws cutline w.r.t. dendrogram y-axis (height), height of line as scalar should be given}
\item{Fast}{If TRUE and fastcluster installed, then a faster implementation of the methods above can be used}
\item{Cls}{[1:n] classification vector for coloring of dendrogram in plot}
\item{\dots}{In case of plotting further argument for \code{plot}, see \code{\link{as.dendrogram}}
}
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

\code{\link{HierarchicalClusterDists}}

\code{\link{HierarchicalClustering}}
}
 \examples{
data('Hepta')
#out=HierarchicalClusterData(Hepta$Data,ClusterNo=7)
}
\keyword{Hierarchical}
\keyword{Clustering}
\keyword{Agglomerative}