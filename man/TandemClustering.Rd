\name{TandemClustering}
\alias{TandemClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tandem Clustering
}
\description{
Summarizes clustering methods that combine k-means clustering and principal component analysis (PCA).
}
\usage{
TandemClustering(Data,ClusterNo,Type="Reduced",PlotIt=FALSE,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix containing the dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{Type}{
 \code{Reduced}: Reduced k-means (RKM) [De Soete/Carroll, 1994].

 \code{Factorial}: Factorial k-means (FKM) [Vichi/Kiers, 2001].

 \code{KernelPCA}: Kernel PCA with minimum normalized-cut hyperplanes [Hofmeyr/Pavlidis, 2019].
}
\item{PlotIt}{Default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}

\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}

\details{
If \code{ClusterNo} exceeds the number of dimensions, the function is called recursively with \code{ClusterNo = 2}. In each iteration, the largest cluster is split again until the requested number of clusters is reached.

\code{"KernelPCA"} additionally uses the \pkg{kernlab} package and follows the fifth example in the extension section on page 18 of [Hofmeyr/Pavlidis, 2019].

The idea of using non-PCA projections for clustering was first proposed by [Bock, 1987], but, to the author's knowledge, it was not applied to data. The first systematic comparison with the projection-pursuit methods \code{\link{ProjectionPursuitClustering}} and \code{\link{AutomaticProjectionBasedClustering}} can be found in [Thrun/Ultsch, 2018].
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
\dontshow{
set.seed(1)
Data <- rbind(
  matrix(rnorm(20, mean = -2), ncol = 2),
  matrix(rnorm(20, mean = 2), ncol = 2)
)
out <- TandemClustering(
  Data,
  ClusterNo = 2,
  Type = "Reduced",
  PlotIt = FALSE
)
}

\donttest{
data("Hepta")
out <- TandemClustering(
  Hepta$Data,
  ClusterNo = 7,
  PlotIt = FALSE
)
}
}
\keyword{TandemClustering}
\concept{Tandem Clustering}