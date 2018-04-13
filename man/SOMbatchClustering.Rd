\name{SOMbatchClustering}
\alias{SOMbatchClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
k-batch self-organizing maps based clustering
}
\description{
...
}
\usage{
SOMbatchClustering(Data,LC=c(3,3),PlotIt=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}
\item{LC}{Lines and Columns of a small SOM, usually every unit is a cluster, 3x3 means 9 units}
\item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\details{
very small maps, not emergent. In approximation every unit can be seen as an cluster.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector defining the classification as the main output of the clustering algorithm}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
R. Wehrens and L.M.C. Buydens, J. Stat. Softw. 21 (5), 2007; R. Wehrens and J. Kruisselbrink, submitted, 2017.
}
\author{
Michael Thrun
}

\examples{
data('Hepta')
out=SOMbatchClustering(Hepta$Data,PlotIt=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{SOM}% use one of  RShowDoc("KEYWORDS")
\keyword{k-batch}% __ONLY ONE__ keyword per line
\keyword{k-batch clustering}% __ONLY ONE__ keyword per line
\keyword{som clustering}% __ONLY ONE__ keyword per line