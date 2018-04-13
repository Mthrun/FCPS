\name{ADPclustering}
\alias{ADPclustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
clustering by fast search and find of density peaks
}
\description{
...
}
\usage{
ADPclustering(Data,PlotIt=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}
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
Rodriguez, A., & Laio, A.:Clustering by fast search and find of density peaks. Science, 344(6191), 1492-1496, 2014.

Xiao-Feng Wang, and Yifan Xu: "Fast Clustering Using Adaptive Density Peak Detection." Statistical Methods in Medical Research, doi:10.1177/0962280215609948, 2015.
}
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
