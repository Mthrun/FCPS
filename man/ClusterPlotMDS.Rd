\name{ClusterPlotMDS}
\alias{ClusterPlotMDS}

\title{
Plot Clustering using MDS
}
\description{
Clustering is plotted with 3D datapoints.
}
\usage{
ClusterPlotMDS(DataOrDists, Cls, main = "Clustering",

method = "euclidean", OutputDimension = 3,

PointSize=1,Plotter3D="rgl", ...)
}

\arguments{
  \item{DataOrDists}{
Either nonsymmetric [1:n,1:d] datamatrix of n cases and d features or symmetric [1:n,1:n] distance matrix
}
  \item{Cls}{
1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
  \item{main}{
String, title of plot
}
  \item{method}{
Method to compute distances, default "euclidean"
}
  \item{OutputDimension}{
Either two or three depending on user choice
}
  \item{PointSize}{
Size of points l
}
  \item{Plotter3D}{
	
In case of 3 dimensions, choose either "plotly" or "rgl",
}
  \item{\dots}{
Please see \code{\link[DataVisualizations]{Plot3D}}
}
}
\details{
If dataset has not more than 3 dimensions, all dimensions are visualized.
If dataset has more than 3 dimesions, Sammons mapping (see Def. in [Thrun, 2018]) is performed and then the first OutputDimension are visualized.
Points are colored by the labels (Cls).
}
\value{
The rgl plot
}
\references{
[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, Heidelberg, Springer, ISBN: 978-3658205393, 2018.
}
\author{
Michael Thrun
}
\note{
If \pkg{DataVisualizations} is not installed a 2D plot using native plot function is shown.

If  \pkg{MASS} is not installed, classicial metric MDS is used, see [Thrun, 2018] for definition.
}

\seealso{
\code{\link[DataVisualizations]{Plot3D}}
}
\examples{
data(Hepta)
ClusterPlotMDS(Hepta$Data,Hepta$Cls)

\donttest{

data(Leukemia)

ClusterPlotMDS(Leukemia$DistanceMatrix,Leukemia$Cls)
}

}

\keyword{ClusterPlotMDS}
