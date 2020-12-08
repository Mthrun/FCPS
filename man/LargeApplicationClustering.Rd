\name{LargeApplicationClustering}
\alias{LargeApplicationClustering}
\title{Large Application Clustering}
\description{
Clustering Large Applications  (clara) of  [Rousseeuw/Kaufman, 1990, pp. 126-163]
}
\usage{
LargeApplicationClustering(Data, ClusterNo,

PlotIt=FALSE,Standardization=TRUE,Samples=50,Random=TRUE,...)
}

\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{Standardization}{
\code{Data} is standardized before calculating the dissimilarities. Measurements are standardized for each variable (column), by subtracting the variable's mean value and dividing by the variable's mean absolute deviation.
}
\item{Samples}{Integer, say N, the number of samples to be drawn from the dataset. Default value set as recommended by documentation of \code{\link[cluster]{clara}}}
\item{Random}{Logical indicating if R's random number generator should be used instead of the primitive clara()-builtin one.}


\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}

\details{
It is recommended to use \code{set.seed} if clustering output should be always the same instead of setting Random=FALSE in order to use the primitive clara()-builtin random number generator.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}

 \examples{
data('Hepta')
out=LargeApplicationClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
}
\author{Michael Thrun}

\references{
[Rousseeuw/Kaufman, 1990]	Rousseeuw, P. J., & Kaufman, L.: Finding groups in data, Belgium, John Wiley & Sons Inc., ISBN: 0471735787, doi 10.1002/9780470316801, Online ISBN: 9780470316801, 1990.
}
\keyword{LargeApplicationClustering}
\keyword{Clustering}
\concept{Large Application Clusteringg}
\keyword{clara}