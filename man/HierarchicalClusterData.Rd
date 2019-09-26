\name{HierarchicalClusterData}
\alias{HierarchicalClusterData}
\alias{HierarchicalCluster}

\title{
Hierarchical Clusterering
}
\usage{
HierarchicalClusterDataData,ClusterNo=0,method="ward.D2",DistanceMethod="euclidean",ColorTreshold=0,...)
}

\description{
Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.Used stats package function 'hclust'.
}
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}
\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}
\item{method}{Methode der Clusterung: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".}
\item{DistanceMethod}{see  \code{\link[parallelDist]{parDist}}, for example 'euclidean','mahalanobis','manhatten' (cityblock),'fJaccard','binary', 'canberra', 'maximum'. Any unambiguous substring can be given.}
\item{ColorTreshold}{draws cutline w.r.t. dendogram y-axis (height), height of line as scalar should be given}
\item{...}{If ClusterNo=0, plot arugments for as.dendrogramm, e.g.leaflab

}
}
\details{
leaflab : a string specifying how leaves are labeled. The default "perpendicular" write text vertically (by default). "textlike" writes text horizontally (in a rectangle), and "none" suppresses leaf labels s. ?as.dendrogramm
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Dedrogram}{Dendrogram of hclust}
}
\author{
Michael Thrun
}

 \examples{
data('Hepta')
out=HierarchicalClusterData(Hepta$Data,ClusterNo=7)
}
\keyword{Hierarchical}% use one of  RShowDoc("KEYWORDS")
\keyword{Clustering}% __ONLY ONE__ keyword per line
\keyword{Agglomerative}% __ONLY ONE__ keyword per line