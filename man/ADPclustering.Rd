\name{ADPclustering}
\alias{ADPclustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
(Adaptive) Density Peak Clustering algorithm using automatic parameter selection
}
\description{
The algorithm was introduced in [Rodriguez/Laio, 2014] and here implemented by [Wang/Xu, 2017]. The algorithm is adaptive in the sense that only \code{ClusterNo} has to be set instead of the paramters of [Rodriguez/Laio, 2014] implemented in \code{\link{ADPclustering}}.
}
\usage{
ADPclustering(Data,ClusterNo=NULL,PlotIt=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}
\item{ClusterNo}{Optional, either: A number k which defines k different Clusters to be build by the algorithm, or a range of \code{ClusterNo} to let the algorithm choose from.}

\item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\details{
The ADP algorithm decides the k number of clusters. This is contrary to the other version of the algorithm from another package which can be called with \code{\link{DensityPeakClustering}}.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Rodriguez/Laio, 2014]  Rodriguez, A., & Laio, A.: Clustering by fast search and find of density peaks, Science, Vol. 344(6191), pp. 1492-1496. 2014.

[Wang/Xu, 2017]  Wang, X.-F., & Xu, Y.: Fast clustering using adaptive density peak detection, Statistical methods in medical research, Vol. 26(6), pp. 2800-2811. 2017.}
\author{
Michael Thrun
}
\seealso{
\code{\link{DensityPeakClustering}}

\code{\link[ADPclust]{adpclust}}
}
\examples{
data('Hepta')
out=ADPclustering(Hepta$Data,PlotIt=FALSE)
}

\keyword{ADPclustering}
\concept{fast search and find of density peaks}
