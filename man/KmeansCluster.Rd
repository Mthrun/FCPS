\name{KmeansCluster}
\Rdversion{1.1}
\alias{KmeansCluster}
\title{
K-Means Clustering
}
\description{
Perform k-means clustering on a data matrix with 100 max iterations.Used stats package function 'kmeans'.
}
\usage{
KmeansCluster(Data, FestgesetzteClustAnz,Centers=FestgesetzteClustAnz, method = 'LBG', Verbose = 0 )
}
\arguments{
  \item{Data}{ der Datensatz in Zeilenvektoren }
	\item{FestgesetzteClustAnz}{the number of clusters, say k}
  \item{Centers}{default(NULL) a set of initial (distinct) cluster centres.}
  \item{method}{ Welcher kmeans Algorithmus verwendet werden sollte. Moegliche Strings sind: "Hartigan", "LBG"}
  \item{Verbose}{ Wenn in Kombination mit method='Hartigan' auf 1 gesetzt, gibt die Funktion mehr Details aus}
  \item{FestgesetzteClustAnz}{fuer Backward Compatibility}
}

\value{

  \item{Cls}{[1:n], k-means Clusterung der Daten}
	\item{SumDistsToCentroids}{Vector of within-cluster sum of squares, one component per cluster}
\item{Centroids}{The final cluster centers.}

}
\references{
Hartigan, J. A. and Wong, M. A.. A K-means clustering algorithm. Applied Statistics 28, 100-108, 1979.
Linde,Y.,Buzo,A.,Gray,R.M., An algorithm for vector quantizer design. IEEE Transactions on Communications, COM-28, 84-95, 1980
}
\author{
Alfred Ultsch, Michael Thrun
}
