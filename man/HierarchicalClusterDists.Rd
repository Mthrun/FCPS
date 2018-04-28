\name{HierarchicalClusterDists}

\alias{HierarchicalClusterDists}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
HierarchicalClusterDists(pDist) HierarchicalClusterDists(pDist,0,"ward.D",100) Cls=HierarchicalClusterDists(pDist,6,"ward.D")    Zeichnet entweder ein Dendrogram oder liefert eine Klassenzuweisung
}
\usage{
HierarchicalClusterDists(pDist,ClusterNo=0,ClusterAlg="ward.D2",ColorTreshold=0)
}

\description{
Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.Used stats package function 'hclust'.
}
\arguments{
\item{pDist}{Distanzen eines Datensatzesueber DistanceMatrix()}
\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}
\item{ClusterAlg}{Methode der Clusterung: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".}
\item{ColorTreshold}{zeichnet Schnittlinie bei entsprechende, Dendogram y-Achsenwerte (Hoehe), Hoehe der Linie wird als Skalar angegeben}
}
\value{
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
}
\author{Michael Thrun}
