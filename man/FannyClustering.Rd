\name{FannyClustering}
\alias{FannyClustering}

\title{
Fuzzy Analysis Clustering [Rousseeuw/Kaufman, 1990, p. 164-198] 
}
\description{
...
}
\usage{
FannyClustering(DataOrDistances,ClusterNo,

PlotIt=FALSE,Standardization=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{DataOrDistances}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features. Alternatively, symmetric [1:n,1:n] distance matrix}

\item{ClusterNo}{A number k which defines k different clusters to be build by the algorithm.}
\item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{Standardization}{
\code{DataOrDistances} is standardized before calculating the dissimilarities. Measurements are standardized for each variable (column), by subtracting the variable's mean value and dividing by the variable's mean absolute deviation. If \code{DataOrDistances} is already a distance matrix, then this argument will be ignored.
}

\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\details{
...
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.
Points which cannot be assigned to a cluster will be reported with 0.
}
\item{Object}{Object defined by clustering algorithm as the second output of this algorithm}
}
\references{
 [Rousseeuw/Kaufman, 1990]	Rousseeuw, P. J., & Kaufman, L.: Finding groups in data, Belgium, John Wiley & Sons Inc., ISBN: 0471735787, 1990.

}
\author{
Michael Thrun
}

\examples{
data('Hepta')
out=FannyClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
}

\keyword{fanny}
\concept{fuzzy clustering}
