\name{DBSclusteringAndVisualization}
\alias{DatabionicSwarmClustering}
\alias{DBSclusteringAndVisualization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Databionic Swarm (DBS) Clustering and Visualization
}
\description{
Swarm-based clustering by exploting self-organization, emergence, swarm intelligence and game theory published in [Thrun/Ultsch, 2021].
}
\usage{
DatabionicSwarmClustering(DataOrDistances, ClusterNo = 0,

StructureType = TRUE, DistancesMethod = NULL,

PlotTree = FALSE, PlotMap = FALSE,PlotIt=FALSE,Data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{DataOrDistances}{
Either nonsymmetric [1:n,1:d] numerical matrix of a dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.

or

symmetric [1:n,1:n] distance matrix, e.g. \code{as.matrix(dist(Data,method))}

}
  \item{ClusterNo}{
Number of Clusters, if zero a the topographic map is ploted. Number of valleys equals number of clusters.
}
  \item{StructureType}{
Either TRUE or FALSE, has to be tested against the visualization. If colored points of clusters a divided by mountain ranges, parameter is incorrect.
}
  \item{DistancesMethod}{
Optional, if data matrix given, annon Euclidean distance can be selected
}
  \item{PlotTree}{
Optional, if TRUE: dendrogram is plotted.
}
  \item{PlotMap}{
Optional, if TRUE: topographic map is plotted if \pkg{GeneralizedUmatrix} is installed.
}
\item{PlotIt}{Default: FALSE, If TRUE and dataset of [1:n,1:d] dimensions then a plot of the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls} will be generated.}

\item{Data}{[1:n,1:d] data matrix in the case that \code{DataOrDistances} is missing and partial matching does not work.}

}
\details{
This function does not enable the user first to project the data and then to test the Boolean parameter defining the type of structure contrary to the \pkg{DatabionicSwarm} which is an inappropriate approach in case of exploratory data analysis. 

Instead, this function is implemented for the purpose of automatic benchmarking because in such a case nobody will investigate many trials with one visualization per trial.

If one would like to perform a clustering exploratively (in the sense that a prior clustering is not given for evaluation purposes), then please use the \pkg{DatabionicSwarm} package directly and read the vignette there.
Databionic swarm is like k-means a stochastic algorithm meaning that the clustering and visualization may change between trials.
}
\value{
List of
\item{Cls}{1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{List of further output of DBS}
}
\references{
[Thrun/Ultsch, 2021]  Thrun, M. C., and Ultsch, A.: Swarm Intelligence for Self-Organized Clustering, Artificial Intelligence, Vol. 290, pp. 103237, \doi{10.1016/j.artint.2020.103237}, 2021.

[Thrun/Ultsch, 2021] Thrun, M. C., & Ultsch, A.: Swarm Intelligence for Self-Organized Clustering (Extended Abstract), in Bessiere, C. (Ed.), 29th International Joint Conference on Artificial Intelligence (IJCAI), Vol. IJCAI-20, pp. 5125--5129, \doi{10.24963/ijcai.2020/720}, Yokohama, Japan, Jan., 2021. 
}
\author{
Michael Thrun
}
\note{
Current implementation is not efficient enough to cluster more than N=4000 cases as in that case it takes longer than a day for a result.
}
\seealso{
\code{\link[DatabionicSwarm]{Pswarm}}, \code{\link[DatabionicSwarm]{DBSclustering}},\code{\link[DatabionicSwarm]{GeneratePswarmVisualization}}
}

\examples{
\donttest{
# Generate random but small non-structured data set
data = cbind(
  sample(1:100, 300, replace = TRUE),
  sample(1:100, 300, replace = TRUE),
  sample(1:100, 300, replace = TRUE)
)
# Make sure there are no structures
# (sample size is small and still could generate structures randomly)
if(requireNamespace('DatabionicSwarm',quietly = TRUE)){
Data = DatabionicSwarm::RobustNormalization(data, Centered = TRUE)
#DataVisualizations::Plot3D(Data)

# No structres are visible
# Topographic map looks like "egg carton"
# with every point in its own valley
Cls = DatabionicSwarmClustering(Data, 0, PlotMap = TRUE)
}else{
# only for testing purposes of CRAN!
# in case CRAN tests with no suggest packages available
# please use alpways some kind of standardization!
Cls = DatabionicSwarmClustering(data, 0, PlotMap = TRUE)
}
}

# Distance based cluster structures
# 7 valleys are visible, thus ClusterNo=7
\donttest{
data(Hepta)
#DataVisualizations::Plot3D(Hepta$Data)

Cls = DatabionicSwarmClustering(Hepta$Data, 0, PlotMap = TRUE)
}

#entagled, complex, and non-linear seperable structures 
\dontrun{
#takes too long for CRAN tests
data(Chainlink)
#DataVisualizations::Plot3D(Chainlink$Data)

# 2 valleys are visible, thus ClusterNo=2
Cls = DatabionicSwarmClustering(Chainlink$Data, 0, PlotMap = TRUE)

# Experiment with parameter StructureType only
# reveals that clustering is appropriate
# if StructureType=FALSE
Cls = DatabionicSwarmClustering(Chainlink$Data,
                                2,
                                StructureType = FALSE,
                                PlotMap = TRUE)

# Here clusters (colored points)
# are not seperated by valleys
Cls = DatabionicSwarmClustering(Chainlink$Data,
                                2,
                                StructureType = TRUE,
                                PlotMap = TRUE)
}

}

\keyword{DBS}
\keyword{DatabionicSwarm}
\keyword{Clustering}
\keyword{swarm}
\keyword{databionic}
\concept{Databionic swarm}
\concept{generalized Umatrix}
\concept{cluster analysis}