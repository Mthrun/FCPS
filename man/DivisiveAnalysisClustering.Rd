\name{DivisiveAnalysisClustering}
\alias{DivisiveAnalysisClustering}
\title{Large DivisiveAnalysisClustering Clustering}
\description{
Divisive Analysis Clustering (diana) of  [Rousseeuw/Kaufman, 1990, pp. 253-279]
}
\usage{
DivisiveAnalysisClustering(DataOrDistances, ClusterNo,

PlotIt=FALSE,Standardization=TRUE,PlotTree=FALSE,Data,...)
}

\arguments{
\item{DataOrDistances}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features. Alternatively, symmetric [1:n,1:n] distance matrix}

\item{ClusterNo}{A number k which defines k different clusters to be build by the algorithm.
if \code{ClusterNo=0} and \code{PlotTree=TRUE}, the dendrogram is generated instead of a clustering to estimate the numbers of clusters.
}
\item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{Standardization}{
\code{DataOrDistances} Is standardized before calculating the dissimilarities. Measurements are standardized for each variable (column), by subtracting the variable's mean value and dividing by the variable's mean absolute deviation.If \code{DataOrDistances} Is already a distance matrix, then this argument will be ignored.
}
\item{PlotTree}{
TRUE: Plots the dendrogram, FALSE: no plot
}
\item{Data}{[1:n,1:d] data matrix in the case that \code{DataOrDistances} is missing and partial matching does not work.}

\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Dendrogram}{Dendrogram of hierarchical clustering algorithm}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}

 \examples{
data('Hepta')
CA=DivisiveAnalysisClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
\donttest{
print(CA$Object)
plot(CA$Object)
ClusterDendrogram(CA$Dendrogram,7,main='DIANA')
}
}
\author{Michael Thrun}

\references{
[Rousseeuw/Kaufman, 1990]	Rousseeuw, P. J., & Kaufman, L.: Finding groups in data, Belgium, John Wiley & Sons Inc., ISBN: 0471735787, doi: 10.1002/9780470316801, Online ISBN: 9780470316801, 1990.
}
\concept{Divisive Analysis Clustering}
\keyword{diana}
\keyword{DivisiveAnalysisClustering}