\name{kmeansDist}
\alias{kmeansDist}
\title{
k-means Clustering using a distance matrix
}
\description{
Perform k-means clustering on a distance matrix
}
\usage{
kmeansDist(Distance, ClusterNo=2,Centers=NULL,

RandomNo=1,maxIt = 2000, 

PlotIt=FALSE,verbose = F)
}
\arguments{
  \item{Distance}{ Distance matrix. For n data points of the dimension n x n   }
  \item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}

  \item{Centers}{Default(NULL) a set of initial (distinct) cluster centres.}
  
  \item{RandomNo}{If>1: Number of random initializations with searching for minimal SSE is defined by this scalar}
	
  \item{maxIt}{Optional: Maximum number of iterations before the algorithm terminates.}
    \item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
  \item{verbose}{Optional: Algorithm always outputs current iteration.}
}

\value{
  \item{Cls[1:n] }{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
  \item{centerids[1:k]}{Indices of the centroids from which the cluster Cls was created}
}
\author{
Felix Pape, Michael Thrun
}
\note{
Currently an experimental version
}
 \examples{
data('Hepta')
#out=kmeansDist(as.matrix(dist(Hepta$Data)),ClusterNo=7,PlotIt=FALSE,RandomNo = 10)

\donttest{
data('Leukemia')
#as expected does not perform well
#for non-spherical cluster structures:
out=kmeansDist(Leukemia$DistanceMatrix,ClusterNo=6,PlotIt=TRUE,RandomNo=10)
}
}

\keyword{kmeansClustering}
\keyword{Clustering}
\concept{kmeans Clustering}
\keyword{distances}