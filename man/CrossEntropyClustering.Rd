\name{CrossEntropyClustering}
\alias{CrossEntropyClustering}
\title{Cross-Entropy Clustering}

\usage{
CrossEntropyClustering(Data, ClusterNo,PlotIt=FALSE,...)
}

\description{
Neural gas clustering published by [Tabor/Spurek, 2014] and implemented by [Spurek et al., 2017].

}
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}

\details{
Contrary to most of the other implemented algorithms in this package, the results on the easiest clustering challenge of Hepta are unstable for cross-entropy clustering
in the sense that the clustering is not always correct. Reproducibilty experiments should be performed (see [Tabor/Spurek, 2014]).
}
 \examples{
data('Hepta')
out=CrossEntropyClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
}
\author{Michael Thrun}



\references{
[Spurek et al., 2017]  Spurek, P., Kamieniecki, K., Tabor, J., Misztal, K., & Śmieja, M.: R package cec, Neurocomputing, Vol. 237, pp. 410-413. 2017.

[Tabor/Spurek, 2014]  Tabor, J., & Spurek, P.: Cross-entropy clustering, Pattern Recognition, Vol. 47(9), pp. 3046-3059. 2014.

}

\keyword{CrossEntropyClustering}
\concept{Cross-Entropy Clustering}
\concept{Cross-Entropy}