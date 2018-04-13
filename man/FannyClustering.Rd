\name{FannyClustering}
\alias{FannyClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
fuzzy clustering
}
\description{
...
}
\usage{
FannyClustering(Data,ClusterNo,PlotIt=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}
\item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\details{
...
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{FannyObject}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
P. Rousseeuw, A. Struyf, M. Hubert, and M. Maechler. cluster:  Cluster  Analysis Extended Rousseeuw et al. , 2006. URL http://www.R-project.org .  R package version 1.11.
}
\author{
Michael Thrun
}

\examples{
data('Hepta')
out=FannyClustering(Hepta$Data,ClusterNo=7,PlotIt=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{fanny}% use one of  RShowDoc("KEYWORDS")
\keyword{fuzzy clustering}% __ONLY ONE__ keyword per line
