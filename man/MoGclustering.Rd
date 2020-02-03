\name{MoGclustering}
\alias{MoGclustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MoGclustering
}
\description{
call MixtureOfGaussians (MoG) clustering based on Expectation Maximization (EM)
}
\usage{
MoGclustering(Data,ClusterNo=2,method="EM",PlotIt=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}
\item{method}{Initialization by either "EM" oder "kmeans"}
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
[Thrun, 2017]  Thrun, M. C.:A System for Projection Based Clustering through Self-Organization and Swarm Intelligence, (Doctoral dissertation), Philipps-Universitaet Marburg, Marburg, 2017.


}
\note{MoG used in [Thrun, 2017] was renamed to \code{\link{ModelBasedClustering}} in this package.}
\author{
Michael Thrun
}
\seealso{
\code{\link{ModelBasedClustering}}
}

 \examples{
data('Hepta')
out=MoGclustering(Hepta$Data,PlotIt=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{MixtureOfGaussians}% use one of  RShowDoc("KEYWORDS")
\keyword{MoG}% __ONLY ONE__ keyword per line
\keyword{EM clustering}% __ONLY ONE__ keyword per line
\keyword{Expectation Maximization}