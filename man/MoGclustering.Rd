\name{MoGclustering}
\alias{MoGclustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mixture of Gaussians Clustering using EM
}
\description{
MixtureOfGaussians (MoG) clustering based on Expectation Maximization (EM) of [Chen et al., 2012] or algorithms closely resembling EM of [Benaglia/Chauveau/Hunter, 2009].
}
\usage{
MoGclustering(Data,ClusterNo=2,Type,PlotIt=FALSE,Silent=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{Type}{string defining approach to select:  initialization approach of "EM" or "kmeans" of [Chen et al., 2012], or other methods "mvnormalmixEM" [McLachlan/Peel, 2000],
"npEM"[Benaglia et al., 2009] or its extension "mvnpEM"
[Chauveau/Hoang, 2016].
}
\item{PlotIt}{Default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{Silent}{(optional) Boolean: print output or not (Default = FALSE = no
output)}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used, see package mixtools \pkg{EMCluster} or \pkg{mixtools} for details.}

}
\details{
Algorithms for clustering through EM or its close resembles.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Chen et al., 2012]  Chen, W., Maitra, R., & Melnykov, V.: EMCluster: EM Algorithm for Model-Based Clustering of Finite Mixture Gaussian Distribution, R Package, URL http://cran. r-project. org/package= EMCluster, 2012.

[Chauveau/Hoang, 2016]  Chauveau, D., & Hoang, V. T. L.: Nonparametric mixture models with conditionally independent multivariate component densities, Computational Statistics & Data Analysis, Vol. 103, pp. 1-16. 2016.

[Benaglia et al., 2009] Benaglia, T., Chauveau, D., and Hunter, D. R.: An EM-like algorithm for semi-and nonparametric estimation in multivariate mixtures. Journal of Computational and Graphical Statistics, 18(2), pp. 505-526, 2009.

[McLachlan/Peel, 2000] D. McLachlan, G. J. and Peel, D.: Finite Mixture Models, John Wiley and Sons, Inc, 2000.

}
\note{MoG used in [Thrun, 2017] was renamed to \code{\link{ModelBasedClustering}} in this package. \code{Type="mvnormalmixEM"} sometimes fails }
\author{
Michael Thrun
}
\seealso{
\code{\link{ModelBasedClustering}}
}

\examples{
data('Hepta')
Data = Hepta$Data
out=MoGclustering(Data,ClusterNo=7,Type="EM",PlotIt=FALSE)
V=out$Cls

\donttest{
V1 = MoGclustering(Data,ClusterNo=7,Type="mvnpEM")
Cls1 = V1$Cls

V2 = MoGclustering(Data,ClusterNo=7,Type="npEM")
Cls2 = V2$Cls
}

\dontrun{
#does not work always
  V3 = MoGclustering(Data,ClusterNo=7,Type="mvnormalmixEM")
  Cls3 = V3$Cls
}

}

\keyword{MixtureOfGaussians}
\keyword{MoG}% __ONLY ONE__ keyword per line
\concept{EM clustering}% __ONLY ONE__ keyword per line
\concept{Expectation Maximization}