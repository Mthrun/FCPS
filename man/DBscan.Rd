\name{DBscan}
\alias{DBscan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DBscan
}
\description{
DBscan clustering
}
\usage{
DBscan(Data,eps,minPts=5,PlotIt,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}

  \item{eps}{
 Radius [Ester et al., 1996, p. 227] (Nachbahrschaft im R-kugel graph/unit disk graph), size of the epsilon neighborhood.
}
  \item{minPts}{
 number of minimum points in the eps region (for core points). 
 In principle minimum number of points in the unit disk, if the unit disk is within the cluster (core) [Ester et al., 1996, p. 228].
 Default is 5 points.
}
\item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}

}

\value{
List of
\item{Cls}{[1:n]  numerical vector defining the clustering; this classification is the main output of the algorithm. Points which cannot be assigned to a cluster will be reported as members of the noise cluster with NaN.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Ester et al., 1996]  Ester, M., Kriegel, H.-P., Sander, J., & Xu, X.: A density-based algorithm for discovering clusters in large spatial databases with noise, Proc. Kdd, Vol. 96, pp. 226-231, 1996.

}
\author{
Michael Thrun
}

\examples{
data('Hepta')
temp=kmeansClustering(Hepta$Data,ClusterNo=7)
out=DBscan(Hepta$Data,sqrt(min(temp$SumDistsToCentroids)),PlotIt=TRUE)
}
