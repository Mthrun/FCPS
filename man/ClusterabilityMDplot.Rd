\name{ClusterabilityMDplot}
\alias{ClusterabilityMDplot}

\title{
Clusterability MDplot
}
\description{
Clusterability mirrored-density plot. Clusterability aims to quantify the degree of cluster structures [Adolfsson et al., 2019].
A dataset has a high probabilty to possess cluster structures, if the first component of the PCA projection is multimodal [Adolfsson et al., 2019]. As the dip test is less exact than the MDplot [Thrun et al., 2020] , pvalues above 0.05 can be given for MDplots which are clearly multimodal. 

An alternative investigation of clusterability can be performed by inspecting the topographic map of the Generalized U-Matrix for a specfic projection method using the \pkg{ProjectionBasesdClustering} and \pkg{GeneralizedUmatrix} packages on CRAN, see [Thrun/Ultsch, 2021] for details.
}
\usage{
ClusterabilityMDplot(DataOrDistance,Method,

na.rm=FALSE,PlotIt=TRUE,\dots)
}

\arguments{
  \item{DataOrDistance}{
  Either a dataset[1:n,1:d] of n cases and d features or a symmetric distance matrix [1:d,1:d] 
  or multiple data sets or distances in a list
}
  \item{Method}{
  "none" performs no dimension reduction.
  
  "pca" uses the scores from the first principal component.
  
  "distance" computes pairwise distances (using distance_metric as the metric).
}
  \item{na.rm}{Statistical testing will not work with missing values, if TRUE values are imputed with averages}
\item{PlotIt}{TRUE: print plot, otherwise do not plot directly, instead use \code{Handle} for further adjustment}
  \item{\dots}{Further arguments for function\code{MDplot4multiplevectors} of package \pkg{DataVisualizations} like \code{"main"}, and \code{"Ordering"}}
}
\details{
Use the method of [Adolfsson et al., 2019] specified as pca plus dip-test (PCA dip) per default without scaling or standardization of data because this step should never be done automatically. In [Thrun, 2020] the standardization and scaling did not improve the results.

If list is named, than the names of the list will be used and the MDplots will be re-ordered according to multimodality in the plot, otherwise only the pvalues of [Adolfsson et al., 2019] will be the names and the ordering of the MDplots is the same as the list.

Beware, as shown below, this test fails for almost touching clusters of Tetra and is difficult to intepret on WingNut but with overlayed with a roubustly estimated unimodal Gaussian distribution it can be interpreted as multimodal). However,  it does not fail for chaining data contrary to the claim in [Adolfsson et al., 2019].

Based on [Thrun, 2020], the author of this function disagrees with [Adolfsson et al., 2019] as to the preference which clusterablity method should be used because the approach "distance" is not preferable for density-based cluster structures.

}
\value{
List of
\item{Handle}{GGobject, plotter handle of \pkg{ggplot2}}
\item{Pvalue}{One or more p-values of dip test depending on \code{DataOrDistance}}


}
\references{
[Adolfsson et al., 2019]  Adolfsson, A., Ackerman, M., & Brownstein, N. C.: To cluster, or not to cluster: An analysis of clusterability methods, Pattern Recognition, Vol. 88, pp. 13-26. 2019.

[Thrun et al., 2020]  Thrun, M. C., Gehlert, T. & Ultsch, A.: Analyzing the Fine Structure of Distributions, PLoS ONE, Vol. 15(10), pp. 1-66, DOI \doi{10.1371/journal.pone.0238835}, 2020. 

[Thrun/Ultsch, 2021]  Thrun, M. C., and Ultsch, A.: Swarm Intelligence for Self-Organized Clustering, Artificial Intelligence, Vol. 290, pp. 103237, \doi{10.1016/j.artint.2020.103237}, 2021.

[Thrun, 2020]  Thrun, M. C.: Improving the Sensitivity of Statistical Testing for Clusterability with Mirrored-Density Plot, in Archambault, D., Nabney, I. & Peltonen, J. (eds.), Machine Learning Methods in Visualisation for Big Data, The Eurographics Association, \url{https://diglib.eg.org:443/handle/10.2312/mlvis20201102}, Norrkoping, Sweden, May, 2020.

}
\author{
Michael Thrun
}
\note{

"none" seems to call dip.test in clusterabilitytest with high-dimensional data. In that case dip.test just vectorizes the matrix of the data which does not make any sense. Since this could be a bug, the "none" option should not be used.

Imputation does not work for distance matrices. Imputation is still experimental. It is adviced to impute missing values before using this function
}

\examples{
##one dataset
data(Hepta)
\donttest{
ClusterabilityMDplot(Hepta$Data)
}
##multiple datasets
data(Atom)
data(Chainlink)
data(Lsun3D)
data(GolfBall)
data(EngyTime)
data(Target)
data(Tetra)
data(WingNut)
data(TwoDiamonds)

DataV = list(
  Atom = Atom$Data,
  Chainlink = Chainlink$Data,
  Hepta = Hepta$Data,
  Lsun3D = Lsun3D$Data,
  GolfBall = GolfBall$Data,
  EngyTime = EngyTime$Data,
  Target = Target$Data,
  Tetra = Tetra$Data,
  WingNut = WingNut$Data,
  TwoDiamonds = TwoDiamonds$Data
)

\donttest{ClusterabilityMDplot(DataV)}

\dontshow{
ClusterabilityMDplot(DataV[3:4])
}
}

\seealso{
\code{\link[DataVisualizations]{MDplot}}
}
\keyword{Clusterability}
\keyword{MDplot}
