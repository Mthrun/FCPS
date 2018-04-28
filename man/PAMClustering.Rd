\name{PAMClustering}
\alias{PAMClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Partitioning Around Medoids [Rousseeuw/Kaufman, 1990, p. 164-198] 
}
\description{
Partitioning (clustering) of the data into k clusters "around medoids", a more robust version of K-means.
}
\usage{
PAMClustering(Data,ClusterNo,PlotIt=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}
\item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\details{
[Rousseeuw/Kaufman, 1990, chapter 2] or [Reynolds et al., 1992].
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{PAMObject}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
 [Rousseeuw/Kaufman, 1990]	Rousseeuw, P. J., & Kaufman, L.: Finding groups in data, Belgium, John Wiley & Sons Inc., ISBN: 0471735787, 1990.

 [Reynolds et al., 1992]	Reynolds, A., Richards, G., de la Iglesia, B. and Rayward-Smith, V.: Clustering rules: A comparison of partitioning and hierarchical clustering algorithms, Journal of Mathematical Modelling and Algorithms 5, 475–504, DOI:10.1007/s10852-005-9022-1, 1992.
}
\author{
Michael Thrun
}

\examples{
data('Hepta')
out=PAMClustering(Hepta$Data,ClusterNo=7,PlotIt=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{fanny}% use one of  RShowDoc("KEYWORDS")
\keyword{fuzzy clustering}% __ONLY ONE__ keyword per line
