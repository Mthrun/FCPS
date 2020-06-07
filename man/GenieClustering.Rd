\name{GenieClustering}
\alias{GenieClustering}
\title{
Genie Clustering by Gini Index
}
\description{
Outlier Resistant Hierarchical Clustering Algorithm of [Gagolewski/Bartoszuk, 2016].
}
\usage{
GenieClustering(DataOrDistances, ClusterNo = 0,
DistanceMethod="euclidean", ColorTreshold = 0,\dots)
}
\arguments{
\item{DataOrDistances}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features. Alternatively, symmetric [1:n,1:n] distance matrix}

\item{ClusterNo}{A number k which defines k different clusters to be build by the algorithm.}

\item{DistanceMethod}{See  \code{\link[parallelDist]{parDist}}, for example 'euclidean','mahalanobis','manhatten' (cityblock),'fJaccard','binary', 'canberra', 'maximum'. Any unambiguous substring can be given.}

\item{ColorTreshold}{Draws cutline w.r.t. dendogram y-axis (height), height of line as scalar should be given}
\item{\dots}{furter argument to genie like:

\code{thresholdGini}  Single numeric value in [0,1], threshold for the Gini index, 1 gives the standard single linkage algorithm
}
}
\details{
Wrapper for Genie algorithm.
}
\value{
List of
\item{Cls}{If, ClusterNo>0: [1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering. Otherwise for ClusterNo=0: NULL}
\item{Dendrogram}{Dendrogram of hierarchical clustering algorithm}
\item{Object}{Ultrametric tree of hierarchical clustering algorithm}
}
\references{
[Gagolewski/Bartoszuk, 2016]  Gagolewski M., Bartoszuk M., Cena A., Genie: A new, fast, and outlier-resistant hierarchical clustering
algorithm, Information Sciences, Vol. 363, pp. 8-23, 2016.
}
\author{
Michael Thrun
}

\seealso{
\code{\link{HierarchicalClustering}}
}
 \examples{
data('Hepta')
Clust=GenieClustering(Hepta$Data,ClusterNo=7)
}
\keyword{Hierarchical}
\keyword{Clustering}
\keyword{Agglomerative}
