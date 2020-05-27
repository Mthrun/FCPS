\name{MinimalEnergyClustering}
\alias{MinimalEnergyClustering}
\title{
Minimal Energy Clustering 
}
\description{
Hierchical Clustering using the minimal energy approach of [Szekely/Rizzo, 2005].
}
\usage{
MinimalEnergyClustering(DataOrDistances, ClusterNo = 0,
DistanceMethod="euclidean", ColorTreshold = 0,Data,\dots)
}
\arguments{
\item{DataOrDistances}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features. Alternatively, symmetric [1:n,1:n] distance matrix}

\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}

\item{DistanceMethod}{see  \code{\link[parallelDist]{parDist}}, for example 'euclidean','mahalanobis','manhatten' (cityblock),'fJaccard','binary', 'canberra', 'maximum'. Any unambiguous substring can be given.}

\item{ColorTreshold}{draws cutline w.r.t. dendogram y-axis (height), height of line as scalar should be given}
\item{Data}{[1:n,1:d] data matrix in the case that \code{DataOrDistances} is missing and partial matching does not work.}
\item{\dots}{If ClusterNo=0, plot arugments for as.dendrogramm, e.g. leaflab
}
}
\details{
leaflab : a string specifying how leaves are labeled. The default "perpendicular" write text vertically (by default). "textlike" writes text horizontally (in a rectangle), and "none" suppresses leaf labels s. ?as.dendrogramm
}
\value{
List of
\item{Cls}{If, ClusterNo>0: [1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering. Otherwise for ClusterNo=0: NULL}
\item{Dendrogram}{Dendrogram of hierarchical clustering algorithm}
\item{Object}{Ultrametric tree of hierarchical clustering algorithm}
}
\references{
[Szekely/Rizzo, 2005]  Szekely, G. J. and Rizzo, M. L.: Hierarchical Clustering via Joint Between-Within Distances: Extending Ward's Minimum Variance Method, Journal of Classification, 22(2) 151-183.http://dx.doi.org/10.1007/s00357-005-0012-9, 2005.
}
\author{
Michael Thrun
}

\seealso{
\code{\link{HierarchicalClustering}}
}
 \examples{
data('Hepta')
out=MinimalEnergyClustering(Hepta$Data,ClusterNo=7)
}
\keyword{Hierarchical}% use one of  RShowDoc("KEYWORDS")
\keyword{Clustering}% __ONLY ONE__ keyword per line

