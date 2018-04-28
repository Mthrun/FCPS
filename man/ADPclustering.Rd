\name{ADPclustering}
\alias{ADPclustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clustering by fast search and find of density peaks [Rodriguez/Laio, 2014]
}
\description{
The algorithm was introduced in [Rodriguez/Laio, 2014] und reimplemented by [Wang/Xu, 2017].
}
\usage{
ADPclustering(Data,ClusterNo=NULL,PlotIt=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}
\item{ClusterNo}{Optional, Either: A number k which defines k different Clusters to be build by the algorithm, or a range of ClusterNo to let the algorithm choose from.}

\item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\details{
The ADP algorithm decides the k number of clusters.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{ADPobject}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Rodriguez/Laio, 2014]  Rodriguez, A., & Laio, A.: Clustering by fast search and find of density peaks, Science, Vol. 344(6191), pp. 1492-1496. 2014.
[Wang/Xu, 2017]  Wang, X.-F., & Xu, Y.: Fast clustering using adaptive density peak detection, Statistical methods in medical research, Vol. 26(6), pp. 2800-2811. 2017.}
\author{
Michael Thrun
}

\examples{
data('Hepta')
out=ADPclustering(Hepta$Data,PlotIt=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ADPclustering}% use one of  RShowDoc("KEYWORDS")
\keyword{fast search and find of density peaks}% __ONLY ONE__ keyword per line
