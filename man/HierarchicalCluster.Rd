\name{HierarchicalCluster}
\Rdversion{1.1}
\alias{HierarchicalCluster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
HierarchicalCluster(Data) HierarchicalClusterDists(Data,0,"ward.D",NULL,"cosine",100) Cls=HierarchicalCluster(Data,6,"ward.D")   Zeichnet entweder ein Dendrogram oder liefert eine Klassenzuweisung
}
\description{
Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.Used stats package function 'hclust'.
}
\arguments{
\item{Data[d,n]}{Der Datensatz, Ohne NaNs!}
\item{FestgesetzteClustAnz}{in soviele Cluster werden die daten eingeteilt, wenn dieser Wert fehlt oder =0 gesetzt ist, wird ein Dendrogramm gezeichnet}
\item{ClusterAlg}{Methode der Clusterung: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".}
\item{Distanz}{see  DistanceMatrix(), for example 'euclidean','sqEuclidean','mahalanobis','cityblock=manhatten','cosine','chebychev','jaccard','minkowski','manhattan','binary', 'canberra', 'maximum'. Any unambiguous substring can be given.}
\item{ColorTreshold}{zeichnet Schnittlinie bei entsprechenden Dendrogram y-Achsenwerte (Hoehe), Hoehe der Linie wird als Skalar angegeben}
\item{...}{Nur Falls FestgesetzteClustAnz=0, plot argumente fuer as.dendrogramm, z.b.}
\item{leaflab}{a string specifying how leaves are labeled. The default "perpendicular" write text vertically (by default). "textlike" writes text horizontally (in a rectangle), and "none" suppresses leaf labels s. ?as.dendrogramm}
}
\value{
HierarchicalCluster      Hierarchische Clusterung der Daten
}
\author{
MT
}

