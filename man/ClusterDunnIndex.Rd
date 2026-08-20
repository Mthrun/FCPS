\name{ClusterDunnIndex}
\alias{ClusterDunnIndex}

\title{
Dunn Index
}
\description{
Calculates the internal Dunn cluster-validity index for a given clustering
[Dunn, 1974]. No prior classification is required.
}
\usage{
ClusterDunnIndex(Cls,DataOrDistances,

DistanceMethod="euclidean",Silent=TRUE,Force=FALSE,\dots)
}

\arguments{
\item{Cls}{A numerical vector of length n defining the classification. It has k
unique values representing the arbitrary cluster labels.}
\item{DataOrDistances}{Either a [1:n,1:d] data matrix containing n observations
and d variables, or a symmetric [1:n,1:n] dissimilarity matrix. For a data
matrix, pairwise distances are calculated using \code{DistanceMethod}.}
\item{DistanceMethod}{Optional distance method passed to
\code{parallelDist::parDist()} or, when \pkg{parallelDist} is unavailable, to
\code{stats::dist()}. The default is \code{"euclidean"}.}
\item{Silent}{Logical scalar. If \code{TRUE}, suppresses the informational
message printed when distances must be calculated from a data matrix.}
\item{Force}{Logical scalar. If \code{TRUE}, computes the ratio even when the maximum within-cluster distance is near zero, using machine precision as the minimum denominator.}
\item{\dots}{Further arguments passed to \code{parallelDist::parDist()}, for
example a user-defined distance function.}
}
\details{
The Dunn index is defined as
\code{min(InterDist) / max(IntraDist)}. Well-separated clusters usually have a
Dunn index above 1; see [Dunn, 1974] for details.
}
\value{
A list with the following components:
\item{Dunn}{A numeric scalar containing the Dunn index.}
\item{IntraDist}{A numerical vector of length k containing the maximum
within-cluster distance for each cluster.}
\item{InterDist}{A numerical vector of length k containing the minimum
between-cluster distance for each cluster.}
}
\references{
[Dunn, 1974] Dunn, J. C.: Well-separated clusters and optimal fuzzy partitions.
Journal of Cybernetics, Vol. 4(1), pp. 95-104, 1974.
}
\author{
Michael Thrun
}

\examples{
data("Hepta")
Cls <- kmeansClustering(
  Hepta$Data,
  ClusterNo = 7,
  Type = "Hartigan"
)$Cls
ClusterDunnIndex(Cls, Hepta$Data)

\donttest{
ClsWellSeparated <- kmeansClustering(
  Hepta$Data,
  ClusterNo = 7,
  Type = "Steinley"
)$Cls
ClusterDunnIndex(ClsWellSeparated, Hepta$Data)
}
}
\keyword{DunnIndex}
\keyword{dunn}
\keyword{Clustering}
\concept{Dunn Index}
