\name{pdfClustering}
\alias{pdfClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Probability Density Distribution Clustering
}
\description{
Clustering via nonparametric density estimation
}
\usage{
pdfClustering(Data, PlotIt = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}
\item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}

}
\details{
Cluster analysis is performed by the density-based procedures described in Azzalini and Torelli (2007) and Menardi and Azzalini (2014), and summarized in Azzalini and Menardi (2014).
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{MCclustObject}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
Azzalini, A., Menardi, G. (2014). Clustering via nonparametric density estimation: the R package pdfCluster. Journal of Statistical Software, 57(11), 1-26, URL http://www.jstatsoft.org/v57/i11/.

Azzalini A., Torelli N. (2007). Clustering via nonparametric density estimation. Statistics and Computing. 17, 71-80.

Menardi, G., Azzalini, A. (2014). An advancement in clustering via nonparametric density estimation. Statistics and Computing. DOI: 10.1007/s11222-013-9400-x.
}
\author{
Michael Thrun
}

 \examples{
data('Hepta')
out=pdfClustering(Hepta$Data,PlotIt=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{pdfClustering}% use one of  RShowDoc("KEYWORDS")
\keyword{Clustering via nonparametric density estimation}% __ONLY ONE__ keyword per line
