\name{MeanShiftClustering}
\alias{MeanShiftClustering}
\title{Mean Shift Clustering}
\description{
Mean Shift Clustering of  [Cheng, 1995]	
}
\usage{
MeanShiftClustering(Data,

PlotIt=FALSE,...)
}

\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}


\item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}

\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}

\details{
the radius used for search can be specified with the "\code{radius}" parameter. The maximum number of iterations before algorithm termination is controlled with the "\code{max_iterations}" parameter.

If the distance between two centroids is less than the given radius, one will be removed. A radius of 0 or less means an estimate will be calculated and used for the radius. Default value "0" (numeric).
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}

 \examples{
data('Hepta')
out=MeanShiftClustering(Hepta$Data,PlotIt=FALSE)
}
\author{Michael Thrun}

\references{
[Cheng, 1995]	Cheng, Yizong: Mean Shift, Mode Seeking, and Clustering, IEEE Transactions on Pattern Analysis and Machine Intelligence, Vol. 17 (8), pp. 790–799, doi:10.1109/34.400568, 1995.
}
\keyword{MeanShiftClustering}
\keyword{Clustering}
\concept{Large Application Clusteringg}
\keyword{clara}