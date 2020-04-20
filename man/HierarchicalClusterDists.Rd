\name{HierarchicalClusterDists}

\alias{HierarchicalClusterDists}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
HierarchicalClusterDists(pDist) HierarchicalClusterDists(pDist,0,"ward.D",100) Cls=HierarchicalClusterDists(pDist,6,"ward.D")    Zeichnet entweder ein Dendrogram oder liefert eine Klassenzuweisung
}
\usage{
HierarchicalClusterDists(pDist,ClusterNo=0,method="ward.D2",

ColorTreshold=0,Fast=FALSE,\dots)
}

\description{
Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.Used stats package function 'hclust'.
}

\arguments{
\item{pDist}{distances as either matrix [1:n,1:n] or dist object}
\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}
\item{method}{method of cluster analysis: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".}
\item{ColorTreshold}{draws cutline w.r.t. dendogram y-axis (height), height of line as scalar should be given}
\item{Fast}{if TRUE and fastcluster installed, then a faster implementation of the methods above can be used}
\item{\dots}{If ClusterNo=0, plot arugments for as.dendrogramm, e.g.

leaflab
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
\author{Michael Thrun}
 \examples{
data('Hepta')
out=HierarchicalClusterData(Hepta$Data,ClusterNo=7)
}
\keyword{Hierarchical}% use one of  RShowDoc("KEYWORDS")
\keyword{Clustering}% __ONLY ONE__ keyword per line
\keyword{Agglomerative}% __ONLY ONE__ keyword per line