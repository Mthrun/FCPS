\name{ClusterPlotMDS}
\alias{ClusterPlotMDS}

\title{
Plot Clustering using Dimensionality Reduction by MDS
}
\description{
Uses a projection method for dimensionality reduction so that the data can be visualized as two- or three-dimensional points colored by cluster.
}
\usage{
ClusterPlotMDS(DataOrDistances, Cls, main = "Clustering",

DistanceMethod = "euclidean", OutputDimension = 3,

PointSize=1,Plotter3D="rgl",Colorsequence, ...)
}

\arguments{
  \item{DataOrDistances}{
Either nonsymmetric [1:n,1:d] datamatrix of n cases and d features or symmetric [1:n,1:n] distance matrix
}
  \item{Cls}{
1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
  \item{main}{
String, title of plot
}
  \item{DistanceMethod}{
Method to compute distances, default "euclidean"
}
  \item{OutputDimension}{
Either two or three depending on user choice
}
  \item{PointSize}{
Scalar defining the size of points
}
  \item{Plotter3D}{

In case of 3 dimensions, choose either "plotly" or "rgl",
}
  \item{Colorsequence}{
[1:k] character vector of colors. By default, the color sequence defined in \pkg{DataVisualizations} is used
}
  \item{\dots}{
Please see \code{\link[DataVisualizations]{Plot3D}} in \pkg{DataVisualizations}
}
}
\details{


If the dataset has more than three dimensions, MDS is performed as implemented in the \pkg{smacof} package [De Leeuw/Mair, 2011].
If \pkg{smacof} is not installed, classical metric MDS (see the definition in [Thrun, 2018]) is performed.
In both cases, the requested \code{OutputDimension} is used. Points are colored according to the labels in \code{Cls}.

If the dataset has no more than three dimensions, all dimensions are visualized and no dimensionality reduction is performed.
}
\value{
The rgl or plotly plot handler depending on \code{Plotter3D}
}
\references{
[De Leeuw/Mair, 2011]  De Leeuw, J., & Mair, P.: Multidimensional scaling using majorization: SMACOF in R, Journal of statistical Software, Vol. 31(3), pp. 1-30. 2011.

[Thrun, 2018] Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, ISBN: 978-3-658-20539-3, Heidelberg, 2018.
}
\author{
Michael Thrun
}
\note{
If \pkg{DataVisualizations} is not installed a 2D plot using native plot function is shown.

If \pkg{smacof} is not installed, classical metric MDS is used; see [Thrun, 2018] for the definition.
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

\keyword{MDS}
\keyword{Projection}
\concept{Multidimensional scaling}
\concept{Projection Method}