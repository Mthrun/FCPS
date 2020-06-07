\name{ModelBasedClustering}
\alias{ModelBasedClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Model Based Clustering
}
\description{
Calls Model based clustering of [Fraley/Raftery, 2006] which models a  Mixture Of Gaussians (MoG).
}
\usage{
ModelBasedClustering(Data,ClusterNo=2,PlotIt=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}

}
\details{
see [Thrun, 2017, p. 23] or [Fraley/Raftery, 2002] and [Fraley/Raftery, 2006].
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Thrun, 2017]  Thrun, M. C.:A System for Projection Based Clustering through Self-Organization and Swarm Intelligence, (Doctoral dissertation), Philipps-Universitaet Marburg, Marburg, 2017.

[Fraley/Raftery, 2002]  Fraley, C., and Raftery, A. E.: Model-based clustering, discriminant analysis, and density estimation, Journal of the American Statistical Association, Vol. 97(458), pp. 611-631. 2002.

[Fraley/Raftery, 2006]  Fraley, C., and Raftery, A. E.MCLUST version 3: an R package for normal mixture modeling and model-based clustering,DTIC Document, 2006.
}
\author{
Michael Thrun
}
 \examples{
data('Hepta')
out=ModelBasedClustering(Hepta$Data,PlotIt=FALSE)
}
\note{MoGclustering used in [Thrun, 2017] was renamed to \code{\link{ModelBasedClustering}} in this package.}
\seealso{
\code{\link{MoGclustering}}
}

\keyword{MixtureOfGaussians}
\keyword{MoG}
\concept{Model based clustering}
\concept{Mixture Of Gaussians}