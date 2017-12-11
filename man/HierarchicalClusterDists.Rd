\name{HierarchicalClusterDists}
\Rdversion{1.1}
\alias{HierarchicalClusterDists}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
HierarchicalClusterDists(pDist) HierarchicalClusterDists(pDist,0,"ward.D",100) Cls=HierarchicalClusterDists(pDist,6,"ward.D")    Zeichnet entweder ein Dendrogram oder liefert eine Klassenzuweisung
}
\description{
Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.Used stats package function 'hclust'.
}
\arguments{
\item{pDist}{Distanzen eines Datensatzesueber DistanceMatrix()}
\item{FestgesetzteClustAnz}{in soviele Cluster werden die daten eingeteilt, wenn dieser Wert fehlt oder =0 gesetzt ist, wird ein Dendrogramm gezeichnet}
\item{ClusterAlg}{Methode der Clusterung: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".}
\item{ColorTreshold}{zeichnet Schnittlinie bei entsprechende, Dendogram y-Achsenwerte (Hoehe), Hoehe der Linie wird als Skalar angegeben}
}
\value{
HierarchicalCluster      Hierarchische Clusterung der Daten, falls FestgesetzteClustAnz angegeben
}
\author{MT}
