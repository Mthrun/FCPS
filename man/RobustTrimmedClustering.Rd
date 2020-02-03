\name{RobustTrimmedClustering}
\alias{RobustTrimmedClustering}
\title{Robust Trimmed Clustering Clustering}
\description{
Robust Trimmed Clustering Clustering of  Garcia-Escudero (2008)
}
\usage{
RobustTrimmedClustering(Data, ClusterNo,

PlotIt=FALSE,...)
}

\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}
\item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}

\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}

\details{
Currently the easies dataset does not work with default parametrization, please see \code{\link[tclust]{tclust}} for parameter description.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}

\examples{
data('Hepta')
out=RobustTrimmedClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE,restr.fact=1e3)
}
\author{Michael Thrun}

\references{
Garcia-Escudero, L. A., Gordaliza, A., Matran, C., & Mayo-Iscar, A. (2008). A general trimming approach to robust cluster analysis. The Annals of Statistics, 36(3), 1324-1345.

Fritz, H., Garcia-Escudero, L. A., & Mayo-Iscar, A. (2012). tclust: An R package for a trimming approach to cluster analysis. Journal of Statistical Software, 47(12), 1-26.
}