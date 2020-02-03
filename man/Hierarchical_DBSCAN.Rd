\name{Hierarchical_DBSCAN}
\alias{Hierarchical_DBSCAN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hierarchical DBSCAN
}
\description{
Hierarchical DBSCAN clustering [Campello et al., 2015].
}
\usage{
Hierarchical_DBSCAN(Data,minPts,

PlotTree=FALSE,PlotIt=FALSE,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}

  \item{minPts}{
 number of minimum points in the eps region (for core points). 
 In principle minimum number of points in the unit disk, if the unit disk is within the cluster (core) [Ester et al., 1996, p. 228].
 Default is 2.5 percent of points.
}

\item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}

\item{PlotTree}{default: FALSE, If TRUE plots the dendrogram. If minPts is missing, PlotTree is set to TRUE.}

\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}

}
\details{
"computes the hierarchical cluster tree representing density estimates along with the stability-based flat cluster extraction proposed by Campello et al. (2013). HDBSCAN essentially computes the hierarchy of all DBSCAN* clusterings, and then uses a stability-based extraction method to find optimal cuts in the hierarchy, thus producing a flat solution."[Hahsler et al., 2019]

}
\value{
List of
\item{Cls}{[1:n]  numerical vector defining the clustering; this classification is the main output of the algorithm. Points which cannot be assigned to a cluster will be reported as members of the noise cluster with 0.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Campello et al., 2015]  Campello RJGB, Moulavi D, Zimek A, Sander J: Hierarchical density estimates for data clustering, visualization, and outlier detection, ACM Transactions on Knowledge Discovery from Data (TKDD), 10(5), pp. 1-51, 2015.

[Hahsler et al., 2019]   Hahsler M, Piekenbrock M, Doran D: dbscan: Fast Density-Based Clustering with R. Journal of Statistical Software, 91(1), pp. 1-30. doi: 10.18637/jss.v091.i01, 2019
}
\author{
Michael Thrun
}

\examples{
data('Hepta')

out=Hierarchical_DBSCAN(Hepta$Data,PlotIt=FALSE)

}
