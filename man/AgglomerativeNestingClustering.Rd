\name{AgglomerativeNestingClustering}
\alias{AgglomerativeNestingClustering}

\title{
AGNES clustering
}
\description{
Agglomerative hierarchical clustering (AGNES)
}
\usage{
AgglomerativeNestingClustering(DataOrDistances, ClusterNo,

PlotIt = FALSE, Standardization = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{DataOrDistances}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features. Alternatively, symmetric [1:n,1:n] distance matrix}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.
if \code{ClusterNo=0}, the dendrogram is generated instead of a clustering to estimate the numbers of clusters.
}
\item{PlotIt}{Default: FALSE if code{ClusterNo!=0}, If TRUE or code{ClusterNo=0} plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{Standardization}{
\code{DataOrDistances} is standardized before calculating the dissimilarities. Measurements are standardized for each variable (column), by subtracting the variable's mean value and dividing by the variable's mean absolute deviation. If \code{DataOrDistances} is already a distance matrix, then this argument will be ignored.
}

\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}

\value{
List of
\item{Cls}{[1:n] numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Dendrogram}{Dendrogram of hierarchical clustering algorithm}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
Kaufman, L. and Rousseeuw, P.J. (1990). Finding Groups in Data: An Introduction to Cluster Analysis. Wiley, New York.

Anja Struyf, Mia Hubert and Peter J. Rousseeuw (1996) Clustering in an Object-Oriented Environment. Journal of Statistical Software 1. 10.18637/jss.v001.i04

Struyf, A., Hubert, M. and Rousseeuw, P.J. (1997). Integrating Robust Clustering Techniques in S-PLUS, Computational Statistics and Data Analysis, 26, 17--37.

Lance, G.N., and W.T. Williams (1966). A General Theory of Classifactory Sorting Strategies, I. Hierarchical Systems. Computer J. 9, 373--380.

Belbin, L., Faith, D.P. and Milligan, G.W. (1992). A Comparison of Two Approaches to Beta-Flexible Clustering. Multivariate Behavioral Research, 27, 417--433.
}
\author{
Michael Thrun
}

\seealso{
\code{\link[cluster]{agnes}}
}
 \examples{
data('Hepta')
CA=AgglomerativeNestingClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
\donttest{
ClusterDendrogram(CA$Dendrogram,7,main='AGNES clustering')

print(CA$Object)
plot(CA$Object)
}
}

\keyword{agnes}
\concept{Agglomerative Nestingg}
\keyword{clustering}
\keyword{cluster analysis}