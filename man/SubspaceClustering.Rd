\name{SubspaceClustering}
\alias{SubspaceClustering}
\title{Algorithms for Subspace clustering}

\usage{
SubspaceClustering(Data, ClusterNo=2,DimSubspace,

PlotIt=FALSE,Algorithm='orclus',OrclusInitialClustersNo=2*ClusterNo,...)
}

\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}
\item{DimSubspace}{numerical number defining the dimensionality in which clusters should be search in}
\item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{Algorithm}{
'orclus', Subspace Clustering Based on Arbitrarily Oriented Projected Cluster Generation

'ProClus' Subspace Clustering Based on Arbitrarily Oriented Projected Cluster Generatio

}

\item{OrclusInitialClustersNo}{Only for Orclus algorithm: Initial number of clusters (that are computed in the entire data space). Must be greater than k. The number of clusters is iteratively decreased by factor a until the final number of k clusters is reached.}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{NeuralGasObject}{Object defined by clustering algorithm as the other output of this algorithm}
}
\note{
JAVA_HOME has to be set for rJava to the ProClus algorithm.
}
 \examples{
data('Hepta')
out=SubspaceClustering(Hepta$Data,ClusterNo=7,PlotIt=TRUE)
}
\author{Michael Thrun}
