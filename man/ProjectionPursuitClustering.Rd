\name{ProjectionPursuitClustering}
\alias{ProjectionPursuitClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cluster Identification using Projection Pursuit as described in [Hofmeyr/Pavlidis, 2019].
}
\description{
Summarizes recent projection pursuit methods for clustering based on [Hofmeyr/Pavlidis, 2015], [Hofmeyr, 2016] and [Pavlidis et al., 2016]  .
}
\usage{
ProjectionPursuitClustering(Data,ClusterNo,Type="MinimumDensity",

PlotIt=FALSE,PlotSolution=FALSE,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}
\item{Type}{
Either  \code{MinimumDensity}[Pavlidis et al., 2016]

 \code{MaximumClusterbility}[Hofmeyr/Pavlidis, 2015]], or

  \code{NormalisedCut} [Hofmeyr, 2016] or KernelPCA [Hofmeyr/Pavlidis, 2019].
}
\item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}

\item{PlotSolution}{
plots the partioning solution as a tree as described in 
}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}

\details{
The details of the options for projection pursuit and partioning of data are defined in [Hofmeyr/Pavlidis, 2019].

"KernelPCA" uses addtionally the package kernlab an is implemented as given in the fifth example on page 21, section "extension" of [Hofmeyr/Pavlidis, 2019].

The first idea of using non-PCA projections for clustering was published by [Bock, 1987] as an definition. However, to the knowledge of the author it was not applied to any data. The first systematic comparison to Projection-Pursuit Methods \code{\link{ProjectionPursuitClustering}} and \code{\link{AutomaticProjectionBasedClustering}} can be found in [Thrun/Ultsch, 2018]. For PCA-based clustering methods please see \code{\link{TandemClustering}}
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.
. Points which cannot be assigned to a cluster will be reported with 0.
}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Hofmeyr/Pavlidis, 2015]  Hofmeyr, D., & Pavlidis, N.: Maximum clusterability divisive clustering, Proc. 2015 IEEE Symposium Series on Computational Intelligence, pp. 780-786, IEEE, 2015.

[Hofmeyr/Pavlidis, 2019]  Hofmeyr, D., & Pavlidis, N.: PPCI: an R Package for Cluster Identification using Projection Pursuit, The R Journal, 2019.

[Hofmeyr, 2016]  Hofmeyr, D. P.: Clustering by minimum cut hyperplanes, IEEE Transactions on Pattern Analysis and Machine Intelligence, Vol. 39(8), pp. 1547-1560. 2016.

[Pavlidis et al., 2016]  Pavlidis, N. G., Hofmeyr, D. P., & Tasoulis, S. K.: Minimum density hyperplanes, The Journal of Machine Learning Research, Vol. 17(1), pp. 5414-5446. 2016.

[Thrun/Ultsch, 2018]  Thrun, M. C., & Ultsch, A.: Using Projection based Clustering to Find Distance and Density based Clusters in High-Dimensional Data, Journal of Classification, Vol. in revision, 2018.

[Bock, 1987]  Bock, H.: On the interface between cluster analysis, principal component analysis, and multidimensional scaling, Multivariate statistical modeling and data analysis, (pp. 17-34), Springer, 1987.
}
\author{
Michael Thrun
}

\examples{
data('Hepta')
out=ProjectionPursuitClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ProjectionPursuitClustering}% use one of  RShowDoc("KEYWORDS")
\keyword{PPCI}% __ONLY ONE__ keyword per line
