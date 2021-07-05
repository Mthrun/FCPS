\name{ClusterDaviesBouldinIndex}
\alias{ClusterDaviesBouldinIndex}

\title{
Davies Bouldin Index
}
\description{
Internal (i.e. without prior classification) cluster quality measure called Davies Bouldin index for a given clustering published in [Davies/Bouldin, 1979].
}
\usage{
ClusterDaviesBouldinIndex(Cls, Data,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Cls}{
[1:n] numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
  \item{Data}{
	matrix, [1:d,1:n] dataset of d variables and n cases
}

    \item{\dots}{Further arguments passed on to the \code{\link[clusterSim]{index.DB}} function of \code{clusterSim}}
}
\details{
Wrapper for \code{\link[clusterSim]{index.DB}}. Davies Bouldin index is defined in [Davies/Bouldin, 1979]. Best clustering scheme essentially minimizes the Davies-Bouldin index [Davies/Bouldin, 1979].
}
\value{
List of 
  \item{Dunn}{
scalar,Davies Bouldin index
}
  \item{Object}{
further information stored in  \code{\link[clusterSim]{index.DB}}
}
}
\references{
[Davies/Bouldin, 1979]  Davies, D. L., & Bouldin, D. W.: A cluster separation measure, IEEE Transactions on Pattern Analysis and Machine Intelligence, Vol. 1(2), pp. 224-227. doi 10.1109/TPAMI.1979.4766909, 1979.
}
\author{
Michael Thrun
}

\examples{
data("Hepta")
Cls=kmeansClustering(Hepta$Data,ClusterNo = 7,Type="Hartigan")$Cls
ClusterDaviesBouldinIndex(Cls,Hepta$Data)[1]

\donttest{
data("Hepta")
ClsWellSeperated=kmeansClustering(Hepta$Data,ClusterNo = 7,Type="Steinley")$Cls
ClusterDaviesBouldinIndex(ClsWellSeperated,Hepta$Data)[1]
}
}
\keyword{DaviesBouldinIndex}
\keyword{Davies}
\keyword{Bouldin}
\keyword{Clustering}
\concept{Davies Bouldin Index}

