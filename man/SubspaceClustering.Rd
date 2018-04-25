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
'orclus', Subspace Clustering Based on Arbitrarily Oriented Projected Cluster Generation [Aggarwal and Yu, 2000]

'ProClus' ProClus Algorithm for Projected Clustering [Aggarwal and Procopiuc, 1999] 

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
JAVA_HOME has to be set for rJava to the ProClus algorithm (in windows set PATH env. variable to .../bin path of Java. The architecture of R and Java have to match. Java automatically downloads the Java version of the browser which may not be installed in the architecture in R. In such a case choose a Java version manually.
}
\references{
[Aggarwal et al., 1999]  Aggarwal, C. C., Wolf, J. L., Yu, P. S., Procopiuc, C., & Park, J. S.: Fast algorithms for projected clustering, Proc. ACM SIGMoD Record, Vol. 28, pp. 61-72, ACM, 1999.

[Aggarwal/Yu, 2000]  Aggarwal, C. C., & Yu, P. S.: Finding generalized projected clusters in high dimensional spaces, (Vol. 29), ACM, ISBN: 1581132174, 2000.

}
 \examples{
data('Hepta')
out=SubspaceClustering(Hepta$Data,ClusterNo=7,PlotIt=TRUE)
}
\author{Michael Thrun}
