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
kmeansClustering(DataOrDistances, ClusterNo,

 Type = 'LBG',RandomNo=5000, CategoricalData,
 
 PlotIt=FALSE, Verbose = FALSE,\dots )
}
\arguments{
\item{DataOrDistances}{Either nonsymmetric [1:n,1:d] datamatrix of n cases and d numerical features or

symmetric [1:n,1:n] distance matrix}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
  \item{Type}{ 
  Choice of Kmeans algorithm, currently either " \code{Hartigan}" [Hartigan/Wong, 1979], "\code{LBG}" [Linde et al., 1980], "\code{Sparse}" sparse k-means proposed in [Witten/Tibshirani, 2010], "\code{Steinley}" best method of [Steinley/Brusco, 2007] proposed in Steinley 2003, "\code{Lloyd}" [Lloyd, 1982], "\code{Forgy}"[Forgy, 1965], \code{MacQueen} [MacQueen, 1967], \code{kcentroids} [Leisch, 2006], "\code{kprototypes}" [Szepannek, 2018], "\code{Pelleg-moore}" [Pelleg & Moores,2000], \code{Elkan}" [Elkan, 2003], \code{Hamerly}"[Hamerly, 2010] ,\code{Dualtree}"  or \code{Dualtree-covertree} [Curtin, 2017]"
  }
    \item{RandomNo}{Only for " \code{Steinley}" or in case of distance matrix, number of random initializations with searching for minimal SSE, see [Steinley/Brusco, 2007]}
    
        \item{CategoricalData}{Only for " \code{kprototypes}", [1:n,1:m] matrix of categorical features]}
        
  \item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
  \item{Verbose}{Print details, if true}
  \item{\dots}{Further arguments like \code{iter.max}, \code{nstart}, for \code{kcentroids} please see \code{kcca} function of the \pkg{flexclust} package, or  \code{\link[sparcl]{KMeansSparseCluster}} 
}
}

\details{
Uses either \pkg{stats} package function 'kmeans', \pkg{cclust} package implemention, \pkg{flexclust} package implemention or own code.
In case of a distance matrix, RandomNo should be significantly lower than 5000, otherwise a long computation time is to be expected.
}
\value{
List V of 

\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.
}

\item{Object}{ Object of the clustering algorithm used if existent, otherwise

SumDistsToCentroids: Vector of within-cluster sum of squares, one component per cluster
}
\item{Centroids}{the final cluster centers.}

}

\references{
[Hartigan/Wong, 1979]  Hartigan, J. A., & Wong, M. A.: Algorithm AS 136: A k-means clustering algorithm, Journal of the Royal Statistical Society. Series C (Applied Statistics), Vol. 28(1), pp. 100-108. 1979.

[Linde et al., 1980]  Linde, Y., Buzo, A., & Gray, R.: An algorithm for vector quantizer design, IEEE Transactions on communications, Vol. 28(1), pp. 84-95. 1980.

[Steinley/Brusco, 2007]  Steinley, D., & Brusco, M. J.: Initializing k-means batch clustering: A critical evaluation of several techniques, Journal of Classification, Vol. 24(1), pp. 99-121. 2007.

[Forgy, 1965]  Forgy, E. W.: Cluster analysis of multivariate data: efficiency versus interpretability of classifications, Biometrics, Vol. 21, pp. 768-769. 1965.

[MacQueen, 1967]  MacQueen, J.: Some methods for classification and analysis of multivariate observations, Proc. Proceedings of the fifth Berkeley symposium on mathematical statistics and probability, Vol. 1, pp. 281-297, Oakland, CA, USA., 1967.

[Pelleg & Moores,2000] Pelleg, Dan, and Andrew W. Moore. X-means: Extending k-means with efficient estimation of the number of clusters, ICML. Vol. 1. 2000.

[Elkan, 2003] Elkan, Charles: Using the triangle inequality to acceler- ate k-means, In Tom Fawcett and Nina Mishra, editors, ICML, pages Vol.3, 147-153. AAAI Press, 2003.

[Lloyd, 1982]  Lloyd, S.: Least squares quantization in PCM, IEEE transactions on information theory, Vol. 28(2), pp. 129-137. 1982.

[Leisch, 2006]  Leisch, F.: A toolbox for k-centroids cluster analysis, Computational Statistics & Data Analysis, Vol. 51(2), pp. 526-544. 2006.

[Witten/Tibshirani, 2010] Witten, D. and Tibshirani, R.: A Framework for
Feature Selection in Clustering. Journal of the American Statistical
Association, Vol. 105(490), pp. 713-726, 2010.

[Hamerly, 2010]  Hamerly, Greg: Making k-means even faster, Proceedings of the 2010 SIAM international conference on data mining, Society for Industrial and Applied Mathematics, pp. 130-140, 2010.

[Szepannek, 2018] Szepannek, G.: clustMixType: User-Friendly Clustering of Mixed-Type Data in R, The R Journal, Vol. 10/2, pp. 200-208, doi:10.32614/RJ2018048, 2018.

[Curtin, 2017]  Curtin, Ryan R: A dual-tree algorithm for fast k-means clustering with large k, Proceedings of the 2017 SIAM International Conference on Data Mining, Society for Industrial and Applied Mathematics, 2017.

}
 \examples{
data('Hepta')
out=kmeansClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)

\donttest{
data('Leukemia')
# As expected does not perform well
# For non-spherical cluster structures:
out=kmeansClustering(Leukemia$DistanceMatrix,ClusterNo=6,RandomNo =10,PlotIt=TRUE)
}
\dontshow{
data('Hepta')
out=kmeansClustering(Hepta$Data,ClusterNo=7,  
PlotIt=FALSE,Type="Hartigan")

out=kmeansClustering(Hepta$Data,ClusterNo=7,  
PlotIt=FALSE,Type="Lloyd")

out=kmeansClustering(Hepta$Data,ClusterNo=7,  
PlotIt=FALSE,Type="Forgy")

out=kmeansClustering(Hepta$Data,ClusterNo=7,  
PlotIt=FALSE,Type="MacQueen")

#out=kmeansClustering(as.matrix(dist(Hepta$Data)),ClusterNo=7,  
#PlotIt=FALSE,RandomNo = 10)
}

\donttest{
data('Hepta')
out=kmeansClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE,Type="Steinley")
}

\donttest{
data('Hepta')
out=kmeansClustering(Hepta$Data,ClusterNo = 7,
Type = "kprototypes",CategoricalData = as.matrix(Hepta$Cls))
}

}

\note{
The version using a distance matrix is still in the test phase and not yet verified.
}

\author{
Michael Thrun
}

\keyword{kmeansClustering}
\keyword{Clustering}
\concept{kmeans Clustering}