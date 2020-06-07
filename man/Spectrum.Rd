\name{Spectrum}
\alias{Spectrum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fast Adaptive Spectral Clustering [John et al, 2020]
}
\description{
Spectrum is a self-tuning spectral clustering method for single or multi-view data. In this wrapper restricted to the standard use in other clustering algorithms.
}
\usage{
Spectrum(Data, Method = 2, ClusterNo = NULL, 

PlotIt = FALSE, Silent = TRUE,PlotResults = FALSE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.
}
  \item{Method}{
Method=1: default eigengap method (Gaussian clusters)

Method=2: multimodality gap method (Gaussian/ non-Gaussian clusters)

Method=3: Allows to setClusterNo
}
  \item{ClusterNo}{Optional, A number k which defines k different clusters to be built by the algorithm.
For default \code{ClusterNo=NULL} please see details.
}

\item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}

  \item{Silent}{
Silent progress of algorithm=TRUE
}
  \item{PlotResults}{
Plots result of spectrum with plot function
}
  \item{\dots}{
Method: numerical value: 1 = default eigengap method (Gaussian clusters), 2 = multimodality
gap method (Gaussian/ non-Gaussian clusters), 3 = no automatic
method (see fixk param)

Other parameters defined in Spectrum packages
}
}
\details{
Spectrum is a partitioning algorithm and either uses the eigengap or multimodality gap heuristics to determine the number of clusters, please see Spectrum package for details
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[John et al, 2020] John, C. R., Watson, D., Barnes, M. R., Pitzalis, C., & Lewis, M. J.: Spectrum: Fast density-aware spectral clustering for single and multi-omic data. Bioinformatics, Vol. 36(4), pp. 1159-1166, 2020.
}
\author{
Michael Thrun
}


\seealso{
\code{\link[Spectrum]{Spectrum}}
}
 \examples{
data('Hepta')
out=Spectrum(Hepta$Data,PlotIt=FALSE)
\donttest{
out=Spectrum(Hepta$Data,PlotIt=TRUE)
}
}

\keyword{Spectrum}
\keyword{FCPS}
\concept{Spectral Clustering}