\name{OPTICSclustering}
\alias{OPTICSclustering}
\title{
OPTICS Clustering
}
\description{
OPTICS (Ordering points to identify the clustering structure) clustering algorithm [Ankerst et al.,1999].
}
\usage{
OPTICSclustering(Data, MaxRadius,RadiusThreshold, minPts = 5, PlotIt=FALSE,\dots)
}
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

  \item{MaxRadius}{
Upper limit neighborhood in the R-ball graph/unit disk graph), size of the epsilon neighborhood  (eps) [Ester et al., 1996, p. 227].
 If NULL, automatic estimation is done using insights of [Ultsch, 2005].
}
  \item{RadiusThreshold}{
Threshold to identify clusters (RadiusThreshold <= MaxRadius), if NULL \code{0.9*MaxRadius} is set.
}
  \item{minPts}{
 Number of minimum points in the eps region (for core points). 
 In principle minimum number of points in the unit disk, if the unit disk is within the cluster (core) [Ester et al., 1996, p. 228].
 If NULL, its 2.5 percent of points.
}
\item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\details{
...
}
\value{
List of
\item{Cls}{[1:n]  numerical vector defining the clustering; this classification is the main output of the algorithm. Points which cannot be assigned to a cluster will be reported as members of the noise cluster with 0.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Ankerst et al.,1999]   Mihael Ankerst, Markus M. Breunig, Hans-Peter Kriegel, Joerg Sander: OPTICS: Ordering Points To Identify the Clustering Structure, ACM SIGMOD international conference on Management of data, ACM Press, pp. 49-60, 1999.

[Ester et al., 1996] Ester, M., Kriegel, H.-P., Sander, J., & Xu, X.: A density-based algorithm for discovering clusters in large spatial databases with noise, Proc. Kdd, Vol. 96, pp. 226-231, 1996.

[Ultsch, 2005] Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, In Baier, D. & Werrnecke, K. D. (Eds.), Innovations in classification, data science, and information systems, (Vol. 27, pp. 91-100), Berlin, Germany, Springer, 2005.
}
\author{
Michael Thrun
}
\seealso{
 \code{\link[dbscan]{optics}}
}
\examples{
data('Hepta')
out=OPTICSclustering(Hepta$Data,MaxRadius=NULL,RadiusThreshold=NULL,minPts=NULL,PlotIt = FALSE)
}
\keyword{optics}% use one of  RShowDoc("KEYWORDS")
\keyword{Clustering}