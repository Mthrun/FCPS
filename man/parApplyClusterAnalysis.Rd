\name{parApplyClusterAnalysis}
\alias{parApplyClusterAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply Operations using Workes for Cluster Analysis
}
\description{
The functions provides several ways to parallelize clustering algorithms using a collection of workers,
}
\usage{
parApplyClusterAnalysis(DataOrDistance, FUN, 

NumberOfTrials = 1:100, ClusterNo = NULL, 

NoWorkers, Type = "PSOCK", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataOrDistance}{
Either [1:N,1:d] Matrix of Data (N cases, d dimensions) that will be used. One DataPoint per row

or symmetric Distance matrix [1:N,1:N] depending on \code{FUN}
}
  \item{FUN}{
function of clustering algorithms. 
}
  \item{NumberOfTrials}{
Number Of Trials to be performed with \code{FUN}
}
  \item{ClusterNo}{
Number of k clusters, if required in \code{FUN}
}
  \item{NoWorkers}{
Number of Workers (number of cores used). 
}
  \item{Type}{
see \code{\link[parallel]{makeCluster}} for details, if default does not work.
}
  \item{\dots}{
further arguments required in \code{FUN}. 
}
}
\details{
Default is the number of cores existing minus 1. In \pkg{FCPS} default parameters for each clustering algorithm are useda automatically if not specified by the user. \code{parApplyClusterAnalysis} expects in \code{FUN} a function of a clustering algorithm which returns a list of objects of which one is named \code{Cls}. If not given the whole output of \code{FUN} is return with a warning.
\code{Cls} is a [1:N] numerical vector of k clusters.
}
\value{
List of
\item{Cls_Matrix}{[1:N,1:NumberOfTrials] numerical matrix consisting of columns, in which each column represents a \code{Cls} vector defining the clustering.}
\item{ComputationTime}{[1:N] numerical vector of the computation time in seconds}
}
\references{
Thrun, M. C.: Projection-Based Clustering through Self-Organization and Swarm Intelligence , Springer, Heidelberg, ISBN: 978-3658205393, 2018. 
}
\author{
Michael Thrun
}

\seealso{
\code{\link[parallel]{clusterApply}}
}
\examples{
\donttest{
Result=parApplyClusterAnalysis(FCPS$Hepta$Data,

kmeansClustering,1:3,7,method="LBG")
#clustering accuracy

apply(Result$Cls_Matrix,2,ClusteringAccuracy,FCPS$Hepta$Cls)
}
}
\keyword{clustering}% use one of  RShowDoc("KEYWORDS")
\keyword{parallel cluster analysis}% __ONLY ONE__ keyword per line
