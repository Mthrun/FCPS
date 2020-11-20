\name{AutomaticProjectionBasedClustering}
\alias{AutomaticProjectionBasedClustering}

\title{
Automatic Projection-Based Clustering
}
\description{
Performs projection-based clustering without user-interaction based on projection methods. The approach is published in [Thrun/Ultsch, 2017], [Thrun/Ultsch, 2020a].
}
\usage{
AutomaticProjectionBasedClustering(DataOrDistances,ClusterNo,Type="NerV",

StructureType = TRUE,PlotIt=FALSE,PlotTree=FALSE,PlotMap=FALSE,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{DataOrDistances}{
Either nonsymmetric [1:n,1:d] numerical matrix of a dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.

or

symmetric [1:n,1:n] distance matrix, e.g. \code{as.matrix(dist(Data,method))}

}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{Type}{
Type of Projection method, either

\code{NerV} [Venna et al., 2010] 

\code{Pswarm} [Thrun/Ultsch, 2020b] 

\code{MDS} [Torgerson, 1952]  
  
\code{Uwot} [McInnes et al., 2018]
    
\code{CCA} [Demartines/Herault, 1995] 
      
\code{Sammon} [Sammon, 1969] 

\code{t-SNE} [Van der Maaten/Hinton, 2008]
}
\item{StructureType}{
Either compact (TRUE) or connected (FALSE), see discussion in [Thrun, 2018] 
}

\item{PlotIt}{Default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}

\item{PlotTree}{
TRUE: Plots the dendrogram, FALSE: no plot
}
\item{PlotMap}{
Plots the topographic map [Thrun et al., 2016].
}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}

\details{
Projection-based clustering \code{\link{AutomaticProjectionBasedClustering}} projects the data (nonlinear) into two dimensions and tries only to preserve relevant neighborhoods prior to clustering. However, in projection-based clustering clustering includes the high-dimensional distances in the clustering process.

The first idea of using non-PCA projections for clustering was published by [Bock, 1987] as a definition. However, to the knowledge of the author, it was not applied to any data. The coexistence of projection and clustering, which exploited the generalized U-Matrix and was introduced in [Thrun/Ultsch, 2017]. It should be noted that it is preferable to use a visualization for the Generalized U-Matrix like the topographic map \code{\link[GeneralizedUmatrix]{plotTopographicMap}} of [Thrun et al., 2016] to evaluate the clustering, improve it or set the number of clusters appropriately. However, a comparison with 32 clustering algorithms showed that PBC is always able to find the correct cluster structure while the best of the 32 clustering algorithms varies depending on the dataset [Thrun/Ultsch, 2020].

The first systematic comparison to other DR clustering methods like Projection-Pursuit Methods \code{\link{ProjectionPursuitClustering}}, supspace clustering methods \code{\link{SubspaceClustering}},  and CA-based clustering methods can be found in [Thrun/Ultsch, 2020a]. For PCA-based clustering methods please see \code{\link{TandemClustering}}.


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

[Thrun/Ultsch, 2020a]  Thrun, M. C., & Ultsch, A.: Using Projection based Clustering to Find Distance and Density based Clusters in High-Dimensional Data, Journal of Classification, in press, doi 10.1007/s00357-020-09373-2, 2020.

[Thrun et al., 2016]  Thrun, M. C., Lerch, F., Loetsch, J., & Ultsch, A.: Visualization and 3D Printing of Multivariate Data of Biomarkers, in Skala, V. (Ed.), International Conference in Central Europe on Computer Graphics, Visualization and Computer Vision (WSCG), Vol. 24, pp. 7-16, Plzen, http://wscg.zcu.cz/wscg2016/short/A43-full.pdf, 2016.

[McInnes et al., 2018]  McInnes, L., Healy, J., & Melville, J.: Umap: Uniform manifold approximation and projection for dimension reduction, arXiv preprint arXiv:1802.03426, 2018.

[Demartines/Herault, 1995]  Demartines, P., & Herault, J.: CCA:" Curvilinear component analysis", Proc. 15 Colloque sur le traitement du signal et des images, Vol. 199, GRETSI, Groupe d Etudes du Traitement du Signal et des Images, France 18-21 September, 1995.

[Sammon, 1969]  Sammon, J. W.: A nonlinear mapping for data structure analysis, IEEE Transactions on computers, Vol. 18(5), pp. 401-409. doi doi:10.1109/t-c.1969.222678, 1969.

[Thrun/Ultsch, 2020b]  Thrun, M. C., & Ultsch, A.: Swarm Intelligence for Self-Organized Clustering, Journal of Artificial Intelligence, Vol. in press, pp. doi 10.1016/j.artint.2020.103237, 2020.

[Torgerson, 1952]  Torgerson, W. S.: Multidimensional scaling: I. Theory and method, Psychometrika, Vol. 17(4), pp. 401-419. 1952.

[Venna et al., 2010]  Venna, J., Peltonen, J., Nybo, K., Aidos, H., & Kaski, S.: Information retrieval perspective to nonlinear dimensionality reduction for data visualization, The Journal of Machine Learning Research, Vol. 11, pp. 451-490. 2010.

[Van der Maaten/Hinton, 2008]  Van der Maaten, L., & Hinton, G.: Visualizing Data using t-SNE, Journal of Machine Learning Research, Vol. 9(11), pp. 2579-2605. 2008.
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
\concept{Projection Based Clustering}