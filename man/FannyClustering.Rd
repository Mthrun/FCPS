\name{FannyClustering}
\alias{FannyClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
fuzzy clustering
}
\description{
...
}
\usage{
FannyClustering(Data,k,PlotIt=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{(1:n,1:m)Data to be clustered. n Datapoints with m Attributes}
\item{k}{Number of different Clusters to build}
\item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of a data set with colored point definded by Cls}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, defaul arguments are used.}
}
\details{
...
}
\value{
List of
\item{Cls}{[1:n]  numerical vector defining the classification as the main output of the clustering algorithm}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
P. Rousseeuw, A. Struyf, M. Hubert, and M. Maechler. cluster:  Cluster  Analysis Extended Rousseeuw et al. , 2006. URL http://www.R-project.org .  R package version 1.11.
}
\author{
Michael Thrun
}

\examples{
data('Hepta')
out=FannyClustering(Hepta$Data,k=7,PlotIt=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{fanny}% use one of  RShowDoc("KEYWORDS")
\keyword{fuzzy clustering}% __ONLY ONE__ keyword per line
