\name{DBSclusteringAndVisualization}
\alias{DatabionicSwarmClustering}
\alias{DBSclusteringAndVisualization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Databionic Swarm (DBS) Clustering and Visualization
}
\description{
Swarm-based clustering by exploting self-organization, emergence, swarm intelligence and game theory.
}
\usage{
DatabionicSwarmClustering(DataOrDistances, ClusterNo = 0,

StructureType = TRUE, DistancesMethod = NULL,

PlotTree = FALSE, PlotMap = FALSE,PlotIt=FALSE,Data)
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
Optional, if TRUE: topographic map is plotted.
}
\item{PlotIt}{default: FALSE, If TRUE and dataset of [1:n,1:d] dimensions then a plot of the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls} will be generated.}

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
\item{Object}{List of further output of DBS}
}
\references{
[Thrun/Ultsch, 2020]  Thrun, M. C., & Ultsch, A.: Swarm Intelligence for Self-Organized Clustering, Journal of Artificial Intelligence, Vol. in press, pp. doi 10.1016/j.artint.2020.103237, 2020.
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
#Generate random but small non-structured data set
data = cbind(
  sample(1:100, 300, replace = T),
  sample(1:100, 300, replace = T),
  sample(1:100, 300, replace = T)
)
#make sure there are no structures
# (sample size is small and still could generate structures randomly)
Data = RobustNormalization(data, Centered = TRUE)
#DataVisualizations::Plot3D(Data)

#No structres are visible
#topographic map looks like "egg carton"
# with every point in its own valley
Cls = DatabionicSwarmClustering(Data, 0, PlotMap = T)
}

#distance based cluster structures
#7 valleys are visible, thus ClusterNo=7
\donttest{
data(Hepta)
#DataVisualizations::Plot3D(Hepta$Data)

Cls = DatabionicSwarmClustering(Hepta$Data, 0, PlotMap = T)
}

#entagled, complex, and non-linear seperable structures 
\donttest{
data(Chainlink)
#DataVisualizations::Plot3D(Chainlink$Data)

#2 valleys are visible, thus ClusterNo=2
Cls = DatabionicSwarmClustering(Chainlink$Data, 0, PlotMap = T)

#Trying of only parameter StructureType
#reveals that clustering is appropriate
# if StructureType=FALSE
Cls = DatabionicSwarmClustering(Chainlink$Data,
                                2,
                                StructureType = FALSE,
                                PlotMap = T)

#Here clusters (colored points)
#are not seperated by valleys
Cls = DatabionicSwarmClustering(Chainlink$Data,
                                2,
                                StructureType = TRUE,
                                PlotMap = T)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{DBS}% use one of  RShowDoc("KEYWORDS")
\keyword{DatabionicSwarm}% __ONLY ONE__ keyword per line
\keyword{Clustering}
\keyword{swarm}
\keyword{databionic}