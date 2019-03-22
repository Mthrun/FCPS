\name{GraphBasedClustering}
\alias{GraphBasedClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MST-kNN clustering algorithm
}
\description{
Performs the MST-kNN clustering algorithm which generate a clustering solution with automatic k determination using two proximity graphs: Minimal Spanning Tree (MST) and k-Nearest Neighbor (kNN) which are recursively intersected.
}
\usage{
GraphBasedClustering(DataOrDistances, method = "euclidean",PlotIt=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{DataOrDistances}{
Either [1:n,1:n] symmetric distance matrix or [1:n,1:d] not symmetric data matrix of n cased and d variable
}

  \item{method}{
Optional distance method of data, default is euclid, see \code{\link[parallelDist]{parDist}} for details
}
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
\item{mstknnclustObject}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
Inostroza-Ponta, M.: An Integrated and Scalable Approach Based on Combinatorial Optimization Techniques for the Analysis of Microarray Data. Ph.D. thesis, School of Electrical Engineering and Computer Science. University of Newcastle, 2008.
}
\author{
Michael Thrun
}


\seealso{
 \code{\link[mstknnclust]{mst.knn}}
}
\examples{
data(Hepta)
\donttest{
GraphBasedClustering(Hepta$Data)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{GraphBasedClustering }% use one of  RShowDoc("KEYWORDS")
\keyword{mst}% __ONLY ONE__ keyword per line
