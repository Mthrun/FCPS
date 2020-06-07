\name{TandemClustering}
\alias{TandemClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tandem Clustering
}
\description{
Summarizes clustering methods that combine k-means and pca
}
\usage{
TandemClustering(Data,ClusterNo,Type="Reduced",PlotIt=FALSE,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{Type}{
 \code{Reduced}: Reduced k-means (RKM) [De Soete/Carroll, 1994].

 \code{Factorial}: Factorial k-mean (FKM) [Vichi/Kiers, 2001] 

 \code{KernelPCA}: Kernel PCA with minimum normalised cut hyperplanes [Hofmeyr/Pavlidis, 2019] 
}
\item{PlotIt}{Default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}

\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}

\details{
If the ClusterNo exceeds the number of dimensions, than the function is called recursively with ClusterNo=2. In each iteration the cluster with the highest number of overall points is clustered again, until the number of clusters is met. 

"KernelPCA" uses addtionally the package kernlab and is implemented as given in the fifth example on page 18, section "extension" of [Hofmeyr/Pavlidis, 2019] 

The first idea of using non-PCA projections for clustering was published by [Bock, 1987] as an definition. However, to the knowledge of the author it was not applied to any data. The first systematic comparison to Projection-Pursuit Methods \code{\link{ProjectionPursuitClustering}} and \code{\link{AutomaticProjectionBasedClustering}} can be found in [Thrun/Ultsch, 2018]. 
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.
Points which cannot be assigned to a cluster will be reported with 0.
}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[De Soete/Carroll, 1994]  De Soete, G., & Carroll, J. D.: K-means clustering in a low-dimensional Euclidean space, New approaches in classification and data analysis, (pp. 212-219), Springer, 1994.

[Hofmeyr/Pavlidis, 2019]  Hofmeyr, D., & Pavlidis, N.: PPCI: an R Package for Cluster Identification using Projection Pursuit, The R Journal, 2019.

[Vichi/Kiers, 2001]  Vichi, M., & Kiers, H. A.: Factorial k-means analysis for two-way data, Computational Statistics & Data Analysis, Vol. 37(1), pp. 49-64. 2001.

[Thrun/Ultsch, 2018]  Thrun, M. C., & Ultsch, A.: Using Projection based Clustering to Find Distance and Density based Clusters in High-Dimensional Data, Journal of Classification, Vol. in revision, 2018.

[Bock, 1987]  Bock, H.: On the interface between cluster analysis, principal component analysis, and multidimensional scaling, Multivariate statistical modeling and data analysis, (pp. 17-34), Springer, 1987.
}

\author{
Michael Thrun
}

\examples{
data('Hepta')
out=TandemClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
}
\keyword{TandemClustering}
\concept{Tandem Clustering}