\name{ClusterStats}
\alias{ClusterStats}
\title{ClusterStats}
\description{
Calulates statistics for clustering in each group of the data points
}
\usage{
ClusterStats(Cls)
}
\arguments{
  \item{Cls}{1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.}
}
\details{If non finite value are given in the numerical vector, the are set to the "9999" cluster}
\value{
\item{UniqueClasses}{ The unique clusters in Cls}
\item{CountPerClass}{ The number of data points in the corresponding unique clusters.}
\item{NumberOfClasses}{ Just that, the number of clusters. }
\item{ClusterPercentages}{percentages in percent per cluster}
}

\references{[Thrun et al., 2018]	Michael C. Thrun, Felix Pape, Alfred Ultsch: Benchmarking Cluster Analysis Methods in the Case of Distance and Density-based Structures Defined by a Prior Classification Using PDE-Optimized Violin Plots, ECDA, Potsdam, 2018
}
\author{Michael Thrun}

\examples{
data('Hepta')
Cls=Hepta$Cls
ClusterStats(Cls)
}


\keyword{ClusterStats}
