\name{MSTclustering}
\alias{MSTclustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MST-kNN clustering algorithm [Inostroza-Ponta, 2008].
}
\description{
Performs the MST-kNN clustering algorithm which generate a clustering solution with automatic k determination using two proximity graphs: Minimal Spanning Tree (MST) and k-Nearest Neighbor (kNN) which are recursively intersected.
}
\usage{
MSTclustering(DataOrDistances, DistanceMethod = "euclidean",PlotIt=FALSE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{DataOrDistances}{
Either [1:n,1:n] symmetric distance matrix or [1:n,1:d] not symmetric data matrix of n cases and d variables
}

  \item{DistanceMethod}{
Optional distance method of data, default is euclid, see \code{\link[parallelDist]{parDist}} for details
}
\item{PlotIt}{Default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
  \item{\dots}{
Optional, further arguments for   \code{\link[mstknnclust]{mst.knn}}
}
}
\details{
Does not work on Hepta with euclidean distances.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Inostroza-Ponta, 2008]  Inostroza-Ponta, M.: An integrated and scalable approach based on combinatorial optimization techniques for the analysis of microarray data, University of Newcastle, ISBN, 2008}
\author{
Michael Thrun
}


\seealso{
 \code{\link[mstknnclust]{mst.knn}}
}
\examples{
data(Hepta)
\donttest{
MSTclustering(Hepta$Data)
}
}

\keyword{MSTclustering}
\keyword{mst}
