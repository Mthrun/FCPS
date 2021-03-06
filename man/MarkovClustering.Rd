\name{MarkovClustering}
\alias{MarkovClustering}

\title{
Markov Clustering
}
\description{
Graph clustering algorithm introduced by [van Dongen, 2000].
}
\usage{
MarkovClustering(DataOrDistances=NULL,Adjacency=NULL,

Radius=TRUE,DistanceMethod="euclidean",addLoops = TRUE,PlotIt=FALSE,\dots)
}

\arguments{
\item{DataOrDistances}{NULL or: Either [1:n,1:n] symmetric distance matrix or [1:n,1:d] not symmetric data matrix of n cases and d variables}
\item{Adjacency}{Used if \code{Data} is NULL, matrix [1:n,1:n] defining which points are adjacent to each other by the number 1; not adjacent: 0}
\item{Radius}{Scalar, Radius for unit disk graph (r-ball graph) if adjacency matrix is missing. Automatic estimation can be done either with =TRUE [Ultsch, 2005] or FALSE [Thrun et al., 2016] if Data instead of Distances are given.}
  \item{DistanceMethod}{
Optional distance method of data, default is euclid, see \code{\link[parallelDist]{parDist}} for details
}
\item{addLoops}{
Logical; if TRUE, self-loops with weight 1 are added to each vertex of x (see \code{mcl} of CRAN package \code{MCL}).
}
\item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\details{
\code{DataOrDistances} is used to compute the \code{Adjecency} matrix if this input is missing. Then a unit-disk (R-ball) graph is calculated.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.
Points which cannot be assigned to a cluster will be reported with 0.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[van Dongen, 2000]  van Dongen, S.M. Graph Clustering by Flow Simulation. Ph.D. thesis, Universtiy of Utrecht. Utrecht University Repository: http://dspace.library.uu.nl/handle/1874/848, 2000

[Thrun et al., 2016]  Thrun, M. C., Lerch, F., Loetsch, J., & Ultsch, A. : Visualization and 3D Printing of Multivariate Data of Biomarkers, in Skala, V. (Ed.), International Conference in Central Europe on Computer Graphics, Visualization  and Computer Vision (WSCG), Vol. 24, Plzen, 2016. 

[Ultsch, 2005]  Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, In Baier, D. & Werrnecke, K. D. (Eds.), Innovations in classification, data science, and information systems, (Vol. 27, pp. 91-100), Berlin, Germany, Springer, 2005.
}
\author{
Michael Thrun
}
\examples{
\dontshow{
data('Hepta')
out=MarkovClustering(Data=Hepta$Data[sample(1:212,10),],PlotIt=FALSE)
}
\donttest{
data('Hepta')
out=MarkovClustering(Data=Hepta$Data,PlotIt=FALSE)
}
}

\concept{Markov Clustering}
\keyword{Markov}
