\name{kmeansClustering}
\Rdversion{1.1}
\alias{kmeansClustering}
\title{
K-Means Clustering
}
\description{
Perform k-means clustering on a data matrix. Uses either stats package function 'kmeans' or cclust package implemention.
}
\usage{
kmeansClustering(Data, ClusterNo,Centers=NULL,

 method = 'LBG',PlotIt=FALSE, Verbose = FALSE,\dots )
}
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}

  \item{Centers}{default(NULL) a set of initial (distinct) cluster centres.}
  \item{method}{ Choice of Kmeans algorithm, currently either "Hartigan" or "LBG"}
  \item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
  \item{Verbose}{ print details, if true}
  \item{\dots}{ Further arguments like \code{iter.max},\code{nstart},...}

}

\value{
List V of 

\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.
}

\item{Object}{ List V of

SumDistsToCentroids: Vector of within-cluster sum of squares, one component per cluster
	
Centroids: the final cluster centers.
}

}

\references{
Hartigan, J. A. and Wong, M. A.. A K-means clustering algorithm. Applied Statistics 28, 100-108, 1979.

Linde,Y.,Buzo,A.,Gray,R.M., An algorithm for vector quantizer design. IEEE Transactions on Communications, COM-28, 84-95, 1980
}
 \examples{
data('Hepta')
out=kmeansClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
}

\author{
Alfred Ultsch, Michael Thrun
}
