\name{PAMclustering}
\alias{PAMclustering}
\alias{PAMClustering}
\title{
Partitioning Around Medoids (PAM)
}
\description{
Partitioning (clustering) of the data into k clusters around medoids, a more robust version of k-means [Rousseeuw/Kaufman, 1990, p. 68-125] .
}
\usage{
PAMclustering(DataOrDistances,ClusterNo,

PlotIt=FALSE,Standardization=TRUE,Data,...)
}
\arguments{
\item{DataOrDistances}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features. Alternatively, symmetric [1:n,1:n] distance matrix}
\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{Standardization}{
\code{DataOrDistances} is standardized before calculating the dissimilarities. Measurements are standardized for each variable (column), by subtracting the variable's mean value and dividing by the variable's mean absolute deviation.If \code{DataOrDistances} is already a distance matrix, then this argument will be ignored.
}
\item{Data}{[1:n,1:d] data matrix in the case that \code{DataOrDistances} is missing and partial matching does not work.}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\details{
[Rousseeuw/Kaufman, 1990, chapter 2] or [Reynolds et al., 1992].
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
 [Rousseeuw/Kaufman, 1990]	Rousseeuw, P. J., & Kaufman, L.: Finding groups in data, Belgium, John Wiley & Sons Inc., ISBN: 0471735787, doi:10.1002/9780470316801, Online ISBN: 9780470316801, 1990.

 [Reynolds et al., 1992]	Reynolds, A., Richards, G.,de la Iglesia, B. and Rayward-Smith, V.: Clustering rules: A comparison of partitioning and hierarchical clustering algorithms, Journal of Mathematical Modelling and Algorithms 5, 475-504, DOI:10.1007/s10852-005-9022-1, 1992.
}
\author{
Michael Thrun
}
\examples{
data('Hepta')
out=PAMclustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
}
\keyword{PAM}
\concept{Partitioning Around Medoids}
\keyword{clustering}