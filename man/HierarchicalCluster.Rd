\name{HierarchicalCluster}

\alias{HierarchicalCluster}

\title{
Hierarchical Clusterering
}
\usage{
HierarchicalCluster(Data,ClusterNo=0,ClusterAlg="ward.D2",Distance="euclidean",ColorTreshold=0,...)
}

\description{
Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.Used stats package function 'hclust'.
}
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}
\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}
\item{ClusterAlg}{Methode der Clusterung: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".}
\item{Distance}{see  DistanceMatrix(), for example 'euclidean','sqEuclidean','mahalanobis','cityblock=manhatten','cosine','chebychev','jaccard','minkowski','manhattan','binary', 'canberra', 'maximum'. Any unambiguous substring can be given.}
\item{ColorTreshold}{zeichnet Schnittlinie bei entsprechenden Dendrogram y-Achsenwerte (Hoehe), Hoehe der Linie wird als Skalar angegeben}
\item{...}{If ClusterNo=0, plot arugments for as.dendrogramm, e.g.

leaflab : a string specifying how leaves are labeled. The default "perpendicular" write text vertically (by default). "textlike" writes text horizontally (in a rectangle), and "none" suppresses leaf labels s. ?as.dendrogramm

}
}
\value{
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}

}
\author{
Michael Thrun
}

 \examples{
data('Hepta')
#out=HierarchicalCluster(Hepta$Data,ClusterNo=7)
}