\name{parApplyClusterAnalysis}
\alias{parApplyClusterAnalysis}

\title{
Apply Operations using Workes for Cluster Analysis
}
\description{
The functions provides several ways to parallelize clustering algorithms using a collection of workers.
It was used for the benchmarking of clustering algorithms in [Thrun, 2018] and [Thrun/Ultsch, 2020].
}
\usage{
parApplyClusterAnalysis(DataOrDistances,FUN,

NumberOfTrials=1:100,ClusterNo=NULL,

WorkersOrNo,SocketType="PSOCK",

SetSeed=TRUE,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataOrDistances}{
Option 1: Either [1:N,1:d] matrix of data (N cases, d dimensions) that will be used. One DataPoint per row

or symmetric distance matrix [1:N,1:N] depending on \code{FUN}.

Option2: A list of data or distance
}
  \item{FUN}{
Function of clustering algorithms. 
}
  \item{NumberOfTrials}{
Number of trials to be performed with \code{FUN}
}
  \item{ClusterNo}{
Number of k clusters, if required in \code{FUN}, if Option2, then a vector of number of k clusters in the same order as the list of DataOrDistance
}
  \item{WorkersOrNo}{
Either already initialized workers using \code{\link[parallel]{makeCluster}} or number of workers (number of cores used). If not set, number is estimated.
}
  \item{SocketType}{
see \code{\link[parallel]{makeCluster}} for details, if default does not work.
}  
\item{SetSeed}{
TRUE: \code{\link{set.seed}} is set to 1000+ (Number Of Trial), \code{ComputationTime} is named with seed
FALSE: \code{\link{set.seed}} is set to NULL), \code{ComputationTime} is named with number of trial
}
  \item{\dots}{
Further arguments required in \code{FUN}. 
}
}
\details{
Default is the number of cores existing minus 1. 

In \pkg{FCPS} default parameters for each clustering algorithm are used automatically if not specified by the user. \code{parApplyClusterAnalysis} expects in \code{FUN} a function of a clustering algorithm which returns a list of objects of which one is named \code{Cls}. If not given the whole output of \code{FUN} is returned with a warning.

\code{Cls} is a [1:N] numerical vector of of numbers 1:k of the k clusters labeling the data points to the clusters.
}
\value{
if Option1

List of
\item{Cls_Matrix}{[1:N,1:NumberOfTrials] numerical matrix consisting of columns, in which each column represents a \code{Cls} vector defining the clustering.}
\item{ComputationTime}{[1:N] numerical vector of the computation time in seconds}

\item{Seeds}{[1:N] seeds used for every trial, if set otherwise NULL}

if Option2: a list of the list defined above named with the names of the DataOrDistance list.
}

\references{
Thrun, M. C.: Projection-Based Clustering through Self-Organization and Swarm Intelligence, Springer, Heidelberg, ISBN: 978-3658205393, 2018. 

Thrun, M. C., & Ultsch, A.: Swarm Intelligence for Self-Organized Clustering, Journal of Artificial Intelligence, in press, 2020. 
}
\author{
Michael Thrun
}

\seealso{
\code{\link[parallel]{clusterApply}}
}
\examples{
\donttest{
data(Hepta)
Result=parApplyClusterAnalysis(Hepta$Data,
kmeansClustering,1:3,7,Type="LBG")
# clustering accuracy

apply(Result$Cls_Matrix,2,ClusteringAccuracy,Hepta$Cls)
}

\dontrun{
data(Hepta)
data(Atom)
DataV=list(Hepta=Hepta$Data,Atom=Atom$Data)
Clusternumbers=c(7,2)
ResultList=parApplyClusterAnalysis(DataV,
kmeansClustering,1:3,ClusterNo=Clusternumbers,Type="LBG")
# clustering accuracy

Accuracies=mapply(FUN=function(x,y) return(apply(x$Cls_Matrix,2,ClusteringAccuracy,y)),ResultList,list(Hepta$Cls,Atom$Cls))
colnames(Accuracies)=names(ResultList)
Accuracies
}
}
\keyword{clustering}% use one of  RShowDoc("KEYWORDS")
\keyword{parallel cluster analysis}% __ONLY ONE__ keyword per line
\keyword{parApply}% __ONLY ONE__ keyword per line
\keyword{CA}% __ONLY ONE__ keyword per line
\keyword{benchmarking}% __ONLY ONE__ keyword per line