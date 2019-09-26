\name{HierarchicalClustering}
\alias{HierarchicalClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hierarchical Clustering
}
\description{
Wrapper for \code{\link{HierarchicalClusterData}} and \code{\link{HierarchicalClusterDists}}.
}
\usage{
HierarchicalClustering(DataOrDistances, ClusterNo,Data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{DataOrDistances}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features. Alternatively, symmetric [1:n,1:n] distance matrix}

\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}
\item{Data}{[1:n,1:d] data matrix in the case that \code{DataOrDistances} is missing and partial matching does not work.}

  \item{\dots}{
Further arguments passed on to either \code{\link{HierarchicalClusterData}} or \code{\link{HierarchicalClusterDists}}.
}
}
\details{
Please see \code{\link{HierarchicalClusterData}} and \code{\link{HierarchicalClusterDists}}.
}
\value{
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}

}

\author{
Michael Thrun
}

\seealso{
\code{\link{HierarchicalClusterData}}

\code{\link{HierarchicalClusterDists}}.
}
 \examples{
data('Hepta')
out=HierarchicalClustering(Hepta$Data,ClusterNo=7)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{HierarchicalClustering}% use one of  RShowDoc("KEYWORDS")
\keyword{Hierarchical}% __ONLY ONE__ keyword per line
\keyword{Clustering}% __ONLY ONE__ keyword per line