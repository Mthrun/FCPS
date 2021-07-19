\name{SparseClustering}
\alias{SparseClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sparse Clustering
}
\description{
Implements the sparse clustering methods of [Witten/Tibshirani, 2010].
}
\usage{
SparseClustering(DataOrDistances, ClusterNo, Type="Hierarchical",

PlotIt=F,Silent=FALSE, NoPerms=10,Wbounds, \dots)
}
\arguments{
\item{DataOrDistances}{Either a [1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.

or a [1:n,1:n] symmetric distance matrix.
}
\item{ClusterNo}{Numeric indicating number to cluster to find in Tree/
Dendrogramm in case of Type="Hierachical" or numer of cluster to use in
Type="kmeans"}
\item{Type}{(optional) Char selecting methods Hierarchical or kmeans.
Default: "Hierarchical"}
\item{PlotIt}{(optional) Boolean. Default = FALSE = No plotting performed.}
\item{Silent}{(optional) Boolean: print output or not (Default = FALSE = no
output)}
\item{NoPerms}{(optional), numeric scalar, Number of permutations.}
\item{Wbounds}{(optional) numeric vector, range of tuning parameters to consider. This is the L1 bound on w, the feature weights [Witten/Tibshirani, 2010].}
\item{\dots}{Further arguments passed on to sparcl \link[sparcl]{HierarchicalSparseCluster} or \link[sparcl]{KMeansSparseCluster} depending on \code{Type}.
}
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
\item{Tree}{Object Tree if Type="Hierachical" is used.}
}
\references{
[Witten/Tibshirani, 2010] Witten, D. and Tibshirani, R.: A Framework for
Feature Selection in Clustering. Journal of the American Statistical
Association, Vol. 105(490), pp. 713-726, 2010.
}
\author{
Quirin Stier, Michael Thrun
}
\note{Quality of clustering results varies between sparse hierarchical if data is given in comparison to the case that distances are given.}
\examples{

# Hepta
data("Hepta")
Data = Hepta$Data
V1 = SparseClustering(Data, ClusterNo=7, Type="kmeans")
Cls1 = V1$Cls

V2 = SparseClustering(Data, ClusterNo=7, Type="Hierarchical")
Cls2 = V2$Cls

InputDistances = parallelDist::parDist(Data, method="euclidean")
DistanceMatrix = as.matrix(InputDistances)
V3 = SparseClustering(DistanceMatrix, ClusterNo=7, Type="Hierarchical")
Cls3 = V3$Cls

\dontrun{
set.seed(1)
Data = matrix(rnorm(100*50),ncol=50)
y    = c(rep(1,50),rep(2,50))
Data[y==1,1:25] = Data[y==1,1:25]+2

V1 = SparseClustering(Data, ClusterNo=2, Type="kmeans")
Cls1 = V1$Cls
}

}
\concept{Sparse Clustering}