\name{NetworkClustering}
\alias{NetworkClustering}

\title{
Network Clustering
}
\description{
Either leiden [Traag et al., 2019] or louvain [Blondel et al., 2008] clustering
}
\usage{
NetworkClustering(DataOrDistances=NULL,Adjacency=NULL,

Type="louvain",Radius=FALSE,PlotIt=FALSE,\dots)
}

\arguments{
\item{DataOrDistances}{NULL or: [1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features. Alternatively, symmetric [1:n,1:n] distance matrix}
\item{Adjacency}{Used if \code{DataOrDistances} is NULL, matrix [1:n,1:n] defining which points are adjacent to each other by the number 1; not adjacent: 0}
\item{Type}{
Either "louvain" or "leiden"
}
\item{Radius}{Scalar, Radius for unit disk graph (r-ball graph) if adjacency matrix is missing. Automatic estimation can be done either with =TRUE [Ultsch, 2005] or FALSE [Thrun et al., 2016]}

\item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\details{
\code{DataOrDistances} is used to compute the \code{Adjecency} matrix if this input is missing. Then a unit-disk (R-ball) graph is calculated.
\code{Radius=TRUE} only works if data matrix is given.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.
Points which cannot be assigned to a cluster will be reported with 0.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Blondel et al., 2008]  Blondel, V. D., Guillaume, J.-L., Lambiotte, R., & Lefebvre, E.: Fast unfolding of communities in large networks, Journal of statistical mechanics: theory and experiment, Vol. 2008(10), pp. P10008. 2008.

[Traag et al., 2019]  Traag, V. A., Waltman, L., & van Eck, N. J.: From Louvain to Leiden: guaranteeing well-connected communities, Scientific reports, Vol. 9(1), pp. 1-12. 2019.
}
\author{
Michael Thrun
}
\examples{
\dontshow{
data('Hepta')
#out=NetworkClustering(Hepta$Data[sample(1:212,10),],PlotIt=FALSE)
}
\donttest{
data('Hepta')
#out=NetworkClustering(Hepta$Data,PlotIt=FALSE)
}
}
\note{
leiden requires igraph package and an installed python version. automatic installation may not work. manual call in console has to be  in this case \code{conda install -c conda-forge leidenalg}
}
\concept{Network Clustering}
\keyword{leiden}
\keyword{louvain}