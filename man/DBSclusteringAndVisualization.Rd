\name{DBSclusteringAndVisualization}
\alias{DBSclusteringAndVisualization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Databionic Swarm (DBS) Clustering and Visualization
}
\description{
Swarm-based clustering by exploting self-organization, emergence, swarm intelligence and game theory.
}
\usage{
DBSclusteringAndVisualization(DataOrDistances, ClusterNo = 0,

StructureType = TRUE, DistancesMethod = NULL,

PlotTree = FALSE, PlotMap = TRUE,Data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataOrDistances}{
Either nonsymmetric [1:n,1:d] datamatrix of n cases and d features or

symmetric [1:n,1:n] distance matrix
}
  \item{ClusterNo}{
Number of Clusters, if zero a the topographic map is ploted. Number of valleys equals number of clusters.
}
  \item{StructureType}{
Either TRUE or FALSE, has to be tested against the visualization. if colored points of clusters a divided by mountain ranges, parameter is incorrect.
}
  \item{DistancesMethod}{
Optional, if data matrix given, annon Euclidean distance can be selected
}
  \item{PlotTree}{
Optional, if TRUE: dendrogram is plotted
}
  \item{PlotMap}{
Optional,if TRUE: topographic map is plotted.
}
\item{Data}{[1:n,1:d] data matrix in the case that \code{DataOrDistances} is missing and partial matching does not work.}

}
\details{
This function does not enable the user first to project the data and then to test the Boolean parameter defining the type of structure contrary to the \pkg{DatabionicSwarm} which is an inappropriate approach in case of exploratory data analysis. 

Instead, this function is implemented for the purpose of automatic benchmarking because in such a case nobody will investigate many trials with one visualization per trial.

If one would like to perform a clustering exploratively (in the sense that a prior clustering is not given of evaluation purposes), then please use the \pkg{DatabionicSwarm} package directly and read the vignette there.
Databionic swarm is like k-means an stochastic algorithm meaning that the clustering and visualization may change between trials.
}
\value{
List of
\item{Cls}{[1:n] numerical vector of k clusters}
\item{DBSobject}{Listof Further output of DBS}
}
\references{
Thrun, M. C., & Ultsch, A.: Swarm Intelligence for Self-Organized Clustering, Journal of Artificial Intelligence, under minor revison, 2019. 
}
\author{
Michael Thrun
}
\note{
Current implementation is not efficient enough to cluster more than N=4000 cases as in that case in takes longer than a day for a result.
}
\seealso{
\code{\link[DatabionicSwarm]{Pswarm}}, \code{\link[DatabionicSwarm]{DBSclustering}},\code{\link[DatabionicSwarm]{GeneratePswarmVisualization}}
}

\examples{
\donttest{
data(Chainlink)
Cls=DBSclusteringAndVisualization(Chainlink$Data,2,
StructureType=FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{DBS}% use one of  RShowDoc("KEYWORDS")
\keyword{DatabionicSwarm}% __ONLY ONE__ keyword per line
\keyword{Clustering}
\keyword{swarm}
\keyword{databionic}