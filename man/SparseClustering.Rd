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
SparseClustering(DataOrDistances, ClusterNo = 0, Type = "Hierarchical",

                 ColorTreshold = 0, Silent = FALSE, NoPerms = 10,

                 Wbounds, PlotIt = FALSE, \dots)
}
\arguments{
\item{DataOrDistances}{Either a [1:n,1:d] matrix containing the dataset to be
clustered, with n cases and d variables, or a symmetric [1:n,1:n] distance
matrix.}
\item{ClusterNo}{Numeric scalar specifying the number of clusters at which to
cut the tree when \code{Type = "Hierarchical"}, or the number of clusters to
construct when \code{Type = "kmeans"}.}
\item{Type}{Optional character scalar selecting \code{"Hierarchical"} or
\code{"kmeans"}. The default is \code{"Hierarchical"}.}
\item{ColorTreshold}{A finite numeric scalar used when
\code{Type = "Hierarchical"}. If it is nonzero and \code{ClusterNo = 0}, the
value is interpreted on the dendrogram height scale. For a monotone
ultrametric hierarchy, \code{stats::rect.hclust()} draws rectangles at this
height, and \code{Cls} is returned from the corresponding cut. If the fusion
heights are non-monotone, only a horizontal reference line is drawn and
\code{Cls = NULL}.}
\item{Silent}{Optional logical scalar passed to the subordinate \pkg{sparcl}
functions' \code{silent} argument. The default is \code{FALSE}.}
\item{NoPerms}{Optional numeric scalar specifying the number of permutations.}
\item{Wbounds}{Optional numeric vector defining the range of tuning parameters
to consider. This is the L1 bound on the feature weights w
[Witten/Tibshirani, 2010].}
\item{PlotIt}{If \code{TRUE} and \code{ClusterNo > 0}, plots the dendrogram
using \code{ClusterDendrogram}. If \code{TRUE} and \code{ClusterNo = 0}, plots
the dendrogram using base R graphics. The default is \code{FALSE}.}
\item{\dots}{Further arguments passed to
\link[sparcl]{HierarchicalSparseCluster} or
\link[sparcl]{KMeansSparseCluster}, depending on \code{Type}.}
}
\value{
A list with the following components:
\item{Cls}{A numerical vector of length n defining the classification. It has k
unique values representing the arbitrary cluster labels.}
\item{Object}{The object returned by the selected clustering algorithm.}
\item{Dendrogram}{The hierarchical clustering tree when
\code{Type = "Hierarchical"}.}
}
\references{
[Witten/Tibshirani, 2010] Witten, D. and Tibshirani, R.: A Framework for
Feature Selection in Clustering. Journal of the American Statistical
Association, Vol. 105(490), pp. 713-726, 2010.
}
\author{
Quirin Stier, Michael Thrun
}
\note{Clustering quality can differ between sparse hierarchical clustering
applied to raw data and the same method applied to precomputed distances.}
\examples{
\dontshow{
set.seed(1)
Data <- rbind(
  matrix(rnorm(20, mean = -2), ncol = 2),
  matrix(rnorm(20, mean = 2), ncol = 2)
)
V <- SparseClustering(
  Data,
  ClusterNo = 2,
  Type = "kmeans",
  NoPerms = 1
)
Cls <- V$Cls
}

\donttest{
# Hepta
data("Hepta")
Data <- Hepta$Data
V1 <- SparseClustering(Data, ClusterNo = 7, Type = "kmeans")
Cls1 <- V1$Cls

V2 <- SparseClustering(Data, ClusterNo = 7, Type = "Hierarchical")
Cls2 <- V2$Cls

InputDistances <- parallelDist::parDist(Data, method = "euclidean")
DistanceMatrix <- as.matrix(InputDistances)
V3 <- SparseClustering(
  DistanceMatrix,
  ClusterNo = 7,
  Type = "Hierarchical"
)
Cls3 <- V3$Cls

set.seed(1)
Data <- matrix(rnorm(100 * 50), ncol = 50)
y <- c(rep(1, 50), rep(2, 50))
Data[y == 1, 1:25] <- Data[y == 1, 1:25] + 2

V1 <- SparseClustering(Data, ClusterNo = 2, Type = "kmeans")
Cls1 <- V1$Cls
}
}
\concept{Sparse Clustering}
