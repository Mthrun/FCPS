\name{kmeansClustering}
\Rdversion{1.1}
\alias{kmeansClustering}
\title{
K-Means Clustering
}
\description{
Perform k-means clustering on a data matrix. 
}
\usage{
kmeansClustering(DataOrDistance, ClusterNo,Centers=NULL,

 Type = 'LBG',RandomNo=5000, PlotIt=FALSE, Verbose = FALSE,\dots )
}
\arguments{
\item{DataOrDistance}{Either nonsymmetric [1:n,1:d] datamatrix of n cases and d features or

symmetric [1:n,1:n] distance matrix}

\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}

  \item{Centers}{default(NULL) a set of initial (distinct) cluster centres.}
  \item{RandomNo}{Only for " \code{Steinley}" or in case of distance matrix, Number of Random Initializations with searching for minimal SSE, see [Steinley/Brusco, 2007]}
  \item{Type}{ Choice of Kmeans algorithm, currently either " \code{Hartigan}" [Hartigan/Wong, 1979], "\code{LBG}" [Linde et al., 1980],"code{Steinley}" best method of [Steinley/Brusco, 2007] proposed in Steinley 2003, " \code{Lloyd}" [Lloyd, 1982], "\code{Forgy}"[Forgy, 1965] or \code{MacQueen} [MacQueen, 1967]}
  \item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
  \item{Verbose}{ print details, if true}
  \item{\dots}{ Further arguments like \code{iter.max},\code{nstart},...}

}

\details{
Uses either stats package function 'kmeans', cclust package implemention or own code.
In case of a distance matrix, RandomNo should be significantly lower than 5000, otherwise a long computation time is to be expected.
}
\value{
List V of 

\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.
}

\item{Object}{ List V of

SumDistsToCentroids: Vector of within-cluster sum of squares, one component per cluster
	
Centroids: the final cluster centers.
}

}

\references{
[Hartigan/Wong, 1979]  Hartigan, J. A., & Wong, M. A.: Algorithm AS 136: A k-means clustering algorithm, Journal of the Royal Statistical Society. Series C (Applied Statistics), Vol. 28(1), pp. 100-108. 1979.

[Linde et al., 1980]  Linde, Y., Buzo, A., & Gray, R.: An algorithm for vector quantizer design, IEEE Transactions on communications, Vol. 28(1), pp. 84-95. 1980.

[Steinley/Brusco, 2007]  Steinley, D., & Brusco, M. J.: Initializing k-means batch clustering: A critical evaluation of several techniques, Journal of Classification, Vol. 24(1), pp. 99-121. 2007.

[Forgy, 1965]  Forgy, E. W.: Cluster analysis of multivariate data: efficiency versus interpretability of classifications, Biometrics, Vol. 21, pp. 768-769. 1965.

[MacQueen, 1967]  MacQueen, J.: Some methods for classification and analysis of multivariate observations, Proc. Proceedings of the fifth Berkeley symposium on mathematical statistics and probability, Vol. 1, pp. 281-297, Oakland, CA, USA., 1967.

[Lloyd, 1982]  Lloyd, S.: Least squares quantization in PCM, IEEE transactions on information theory, Vol. 28(2), pp. 129-137. 1982.
}
 \examples{
data('Hepta')
out=kmeansClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)

\donttest{
data('Leukemia')
#as expected does not perform well
#for non-spherical cluster structures:
out=kmeansClustering(Leukemia$DistanceMatrix,ClusterNo=6,RandomNo =10,PlotIt=TRUE)
}
}

\note{
The version using a distance matrix is is still in the test phase and not verified.
}

\author{
Alfred Ultsch, Michael Thrun
}

\keyword{kmeansClustering}
\keyword{Clustering}
\concept{kmeans Clustering}