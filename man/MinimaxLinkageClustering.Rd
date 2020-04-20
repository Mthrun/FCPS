\name{MinimaxLinkageClustering}
\alias{MinimaxLinkageClustering}
\title{
Minimax Linkage Hierarchical Clustering
}
\description{
inimax linkage hierarchical clustering. Every cluster has an associated prototype element that represents that cluster [Bien/Tibshirani, 2011].
}
\usage{
MinimaxLinkageClustering(DataOrDistances, ClusterNo = 0,
DistanceMethod="euclidean", ColorTreshold = 0,\dots)
}
\arguments{
\item{DataOrDistances}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features. Alternatively, symmetric [1:n,1:n] distance matrix}

\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}

\item{DistanceMethod}{see  \code{\link[parallelDist]{parDist}}, for example 'euclidean','mahalanobis','manhatten' (cityblock),'fJaccard','binary', 'canberra', 'maximum'. Any unambiguous substring can be given.}

\item{ColorTreshold}{draws cutline w.r.t. dendogram y-axis (height), height of line as scalar should be given}
\item{\dots}{If ClusterNo=0, plot arugments for as.dendrogramm, e.g. leaflab
}
}
\details{
Wrapper for Minimax Linkage Hierarchical Clustering algorithm.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Dendrogram}{Dendrogram of hclust, if ClusterNo=0}
\item{Object}{object of protoclust if ClusterNo!=0}
}
\references{
[Bien/Tibshirani, 2011] Bien, J., and Tibshirani, R.: Hierarchical Clustering with Prototypes via Minimax Linkage, The Journal of the American Statistical Association, Vol. 106(495), pp. 1075-1084, 2011.
}
\author{
Michael Thrun
}

\seealso{
\code{\link{HierarchicalClustering}}
}
 \examples{
data('Hepta')
out=MinimaxLinkageClustering(Hepta$Data,ClusterNo=7)
}
\keyword{Hierarchical}% use one of  RShowDoc("KEYWORDS")
\keyword{Clustering}% __ONLY ONE__ keyword per line
\keyword{Agglomerative}% __ONLY ONE__ keyword per line
