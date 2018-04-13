\name{MarkovClustering}
\alias{MarkovClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Markov Clustering
}
\description{
Graph clustering algorithm
}
\usage{
MarkovClustering(Data=NULL,Adjacency=NULL,addLoops =TRUE,PlotIt=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}
\item{Adjacency}{matrix [1:n,1:n] defining which points are adjacent to each other by the number 1; not adjacent: 0}
\item{addLoops}{
logical; if TRUE, self-loops with weight 1 are added to each vertex of x (see \code{mcl} of CRAN package \code{MCL}).
}
\item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\details{
...
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
van Dongen, S.M. Graph Clustering by Flow Simulation. Ph.D. thesis, Universtiy of Utrecht. Utrecht University Repository: http://dspace.library.uu.nl/handle/1874/848, 2000
}
\author{
Michael Thrun
}
\examples{
data('Hepta')
#out=MarkovClustering(Data=Hepta$Data,PlotIt=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Markov Clustering}% use one of  RShowDoc("KEYWORDS")
\keyword{Markov}% __ONLY ONE__ keyword per line