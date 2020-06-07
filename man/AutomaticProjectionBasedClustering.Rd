\name{AutomaticProjectionBasedClustering}
\alias{AutomaticProjectionBasedClustering}

\title{
Automatic Projection-Based Clustering
}
\description{
Summarizes recent projection methods for clustering based on [Thrun/Ultsch, 2017] and [Thrun/Ultsch, 2018] except projection pursuit methods which were published independendly.
}
\usage{
AutomaticProjectionBasedClustering(DataOrDistances,ClusterNo,Type="NerV",

StructureType = TRUE,PlotIt=FALSE,PlotTree=FALSE,PlotMap=FALSE,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{DataOrDistances}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{Type}{
Type of Projection method, either

\code{NerV} [Venna et al., 2010] 

\code{Pswarm} [Thrun/Ultsch, 2020] 

\code{MDS} [Torgerson, 1952]  
  
\code{ICA} [Comon, 1992] 
    
\code{CCA} [Demartines/Herault, 1995] 
      
\code{Sammon} [Sammon, 1969] 
}
\item{StructureType}{
Either compact (TRUE) or connected (FALSE), see discussion in [Thrun, 2018] 
}

\item{PlotIt}{Default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}

\item{PlotTree}{
Plots the dendrogram
}
\item{PlotMap}{
Plots the topographic map [Thrun et al., 2016].
}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}

\details{
Clustering is based on the Generalized U-Matrix [Ultsch/Thrun, 2017]. The first idea of using non-PCA projections for clustering was published by [Bock, 1987] as a definition. However, to the knowledge of the author it was not applied to any data. The first systematic comparison to Projection-Pursuit Methods \code{\link{ProjectionPursuitClustering}} can be found in [Thrun/Ultsch, 2018]. For PCA-based clustering methods please see \code{\link{TandemClustering}}
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.
. Points which cannot be assigned to a cluster will be reported with 0.
}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Bock, 1987]  Bock, H.: On the interface between cluster analysis, principal component analysis, and multidimensional scaling, Multivariate statistical modeling and data analysis, (pp. 17-34), Springer, 1987.

[Thrun/Ultsch, 2017]  Thrun, M. C., & Ultsch, A.: Projection based Clustering, Proc. International Federation of Classification Societies (IFCS), pp. 250-251, Tokai University, Japanese Classification Society (JCS), Tokyo, Japan August 7-10, 2017.

[Thrun/Ultsch, 2018]  Thrun, M. C., & Ultsch, A.: Using Projection based Clustering to Find Distance and Density based Clusters in High-Dimensional Data, Journal of Classification, Vol. in revision, 2018.

[Ultsch/Thrun, 2017]  Ultsch, A., & Thrun, M. C.: Credible Visualizations for Planar Projections, in Cottrell, M. (Ed.), 12th International Workshop on Self-Organizing Maps and Learning Vector Quantization, Clustering and Data Visualization (WSOM), 10.1109/WSOM.2017.8020010, pp. 1-5, IEEE, Nany, France, 2017.

[Thrun et al., 2016]  Thrun, M. C., Lerch, F., Loetsch, J., & Ultsch, A.: Visualization and 3D Printing of Multivariate Data of Biomarkers, in Skala, V. (Ed.), International Conference in Central Europe on Computer Graphics, Visualization and Computer Vision (WSCG), Vol. 24, pp. 7-16, Plzen, http://wscg.zcu.cz/wscg2016/short/A43-full.pdf, 2016.

[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, (Ultsch, A. & Huellermeier, E. Eds., 10.1007/978-3-658-20540-9), Extended and Corrected Version of Doctoral dissertation submitted Jan 2017, Heidelberg, Springer, ISBN: 978-3658205393, 2018.


[Comon, 1992]  Comon, P.: Independent component analysis, Higher-Order Statistics, Vol., pp. 29-38. 1992.

[Demartines/Herault, 1995]  Demartines, P., & Herault, J.: CCA:" Curvilinear component analysis", Proc. 15 Colloque sur le traitement du signal et des images, Vol. 199, GRETSI, Groupe d Etudes du Traitement du Signal et des Images, France 18-21 September, 1995.

[Sammon, 1969]  Sammon, J. W.: A nonlinear mapping for data structure analysis, IEEE Transactions on computers, Vol. 18(5), pp. 401-409. doi doi:10.1109/t-c.1969.222678, 1969.

[Thrun/Ultsch, 2020]  Thrun, M. C., & Ultsch, A.: Swarm Intelligence for Self-Organized Clustering, Journal of Artificial Intelligence, Vol. in press, pp. doi 10.1016/j.artint.2020.103237, 2020.

[Torgerson, 1952]  Torgerson, W. S.: Multidimensional scaling: I. Theory and method, Psychometrika, Vol. 17(4), pp. 401-419. 1952.

[Venna et al., 2010]  Venna, J., Peltonen, J., Nybo, K., Aidos, H., & Kaski, S.: Information retrieval perspective to nonlinear dimensionality reduction for data visualization, The Journal of Machine Learning Research, Vol. 11, pp. 451-490. 2010.


}
\author{
Michael Thrun
}

\examples{
\donttest{
data('Hepta')
out=AutomaticProjectionBasedClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
}
\dontshow{
data('Hepta')
out=AutomaticProjectionBasedClustering(Hepta$Data[1:10,],ClusterNo=2,PlotIt=FALSE)
}
}

\keyword{AutomaticProjectionBasedClustering}% use one of  RShowDoc("KEYWORDS")