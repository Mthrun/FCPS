\name{RandomForestClustering}
\alias{RandomForestClustering}
\alias{RandomForestClustering}
\title{
Random Forest Clustering
}
\description{
Clustering using the proximity matrix of random forest with either PAM or hierarchical clustering algorithms.
}
\usage{
RandomForestClustering(Data,ClusterNo,

method="ward.D2",NoTrees = 2000,

PlotIt=FALSE,PlotForest=FALSE,\dots)
}
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features}
\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{method}{Method of cluster analysis: "PAM", "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".}

\item{NoTrees}{A number of trees used in the forest}

\item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}

\item{PlotForest}{Default: FALSE, If TRUE plots the forest}

\item{\dots}{Further arguments to be set for the random forest algorithm, if not set, default arguments are used.}
}
\details{
Inspired by [Alhusain/Hafez, 2017].
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Alhusain/Hafez, 2017]  Alhusain, L., & Hafez, A. M.: Cluster ensemble based on Random Forests for genetic data, BioData mining, Vol. 10(1), pp. 37. 2017.

}
\author{
Michael Thrun
}
\examples{
data('Hepta')
out=RandomForestClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
}
\keyword{RandomForestClustering}
\concept{Random Forest Clustering}
\keyword{clustering}