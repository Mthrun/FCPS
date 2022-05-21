\name{ClusterDunnIndex}
\alias{ClusterDunnIndex}

\title{
Dunn Index
}
\description{
Internal (i.e. without prior classification) cluster quality measure called Dunn index for a given clustering published in [Dunn, 1974].
}
\usage{
ClusterDunnIndex(Cls,DataOrDistances,

DistanceMethod="euclidean",Silent=TRUE,Force=FALSE,\dots)
}

\arguments{
  \item{Cls}{
[1:n] numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
  \item{DataOrDistances}{
	matrix, DataOrDistance[1:n,1:n] symmetric matrix of dissimilarities, if variable unsymmetric 
	DataOrDistance[1:d,1:n] is assumed as a dataset and the euclidean distances are calculated of d variables and n cases
}

  \item{DistanceMethod}{
Optional, one of 39 distance methods of \code{parDist} of package parallelDist,  if Data matrix is chosen above
}
  \item{Silent}{
TRUE: Warnings are shown
}
  \item{Force}{
TRUE: force computing in case of numerical instability
}
    \item{\dots}{Further arguments passed on to the \code{parDist} function, e.g. user_defined distance functions}
}
\details{
Dunn index is defined as \code{Dunn=min(InterDist)/max(IntraDist)}. Well seperated clusters have usually a dunn index above 1, for details please see [Dunn, 1974].
}
\value{
List of 
  \item{Dunn}{
scalar, Dunn Index
}
  \item{IntraDist}{
[1:k] numerical vector of minimal intra cluster distances per given cluster
}
  \item{InterDist}{
[1:k] numerical vector of minimal inter cluster distances per given cluster
}
}
\references{
[Dunn, 1974]  Dunn, J. C.: Well_separated clusters and optimal fuzzy partitions, Journal of cybernetics, Vol. 4(1), pp. 95-104. 1974.
}
\author{
Michael Thrun
}

\examples{
data("Hepta")
Cls=kmeansClustering(Hepta$Data,ClusterNo = 7,Type="Hartigan")$Cls
ClusterDunnIndex(Cls,Hepta$Data)

\donttest{
data("Hepta")
ClsWellSeperated=kmeansClustering(Hepta$Data,ClusterNo = 7,Type="Steinley")$Cls
ClusterDunnIndex(ClsWellSeperated,Hepta$Data)
}
}
\keyword{DunnIndex}
\keyword{dunn}
\keyword{Clustering}
\concept{Dunn Index}

