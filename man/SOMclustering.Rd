\name{SOMclustering}
\alias{SOMclustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
self-organizing maps based clustering implemented by [Wherens, Buydens, 2017]. 
}
\description{
Either the variant k-batch or k-online is possible in which every unit can be seen approximately as an cluster.
}
\usage{
SOMclustering(Data,LC=c(1,2),ClusterNo=NULL,

Mode="online",PlotIt=FALSE,rlen=100,alpha = c(0.05, 0.01),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}
\item{LC}{Lines and Columns of a very small SOM, usually every unit is a cluster, will be ignored if ClusterNo is not NULL.}
\item{ClusterNo}{Optional, A number k which defines k different clusters to be built by the algorithm. LC will then be set accordingly.}
\item{Mode}{Either "batch" or "online"}
\item{PlotIt}{Default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}

\item{rlen}{Please see \code{\link[kohonen]{supersom}}}
\item{alpha}{Please see \code{\link[kohonen]{supersom}}}
\item{\dots}{Further arguments to be set for the clustering algorithm in 
\code{\link[kohonen:unit.distances]{somgrid}}, if not set, default arguments are used.}
}
\details{
This clustering algorithm is based on very small maps and, hence, not emergent (c.f. [Thrun, 2018, p.37]). A 3x3 map means 9 units leading to 9 clusters.

Batch is a deterministic clustering approach whereas online is a stochastic clustering approach and research indicates that online should be preferred (c.f. [Thrun, 2018, p.37]).
}
\value{
List of
\item{Cls}{[1:n]  numerical vector defining the classification as the main output of the clustering algorithm}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Wherens, Buydens, 2017] R. Wehrens and L.M.C. Buydens, J. Stat. Softw. 21 (5), 2007; R. Wehrens and J. Kruisselbrink, submitted, 2017.

[Thrun, 2018] Thrun, M.C., Projection Based Clustering through Self-Organization and Swarm Intelligence. 2018, Heidelberg: Springer.
}
\author{
Michael Thrun
}

\examples{
data('Hepta')
out=SOMclustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
}

\keyword{SOM}
\keyword{k-batch}
\concept{k-batch clustering}
\concept{som clustering}