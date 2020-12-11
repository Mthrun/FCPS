\name{SubspaceClustering}
\alias{SubspaceClustering}
\title{Algorithms for Subspace clustering}
\description{
Subspace (projected) clustering is a technique which finds clusters within different subspaces (a selection of one or more dimensions).
}
\usage{
SubspaceClustering(Data,ClusterNo,DimSubspace,

Type='Orclus',PlotIt=FALSE,OrclusInitialClustersNo=ClusterNo+2,...)
}
\details{
Subspace clustering algorithms have the goal to finde one or more subspaces with the assumation that sufficient dimensionality reduction is dimensionality reduction without loss of information. Hence subspace clustering aums at finding a linear subspace sucht that the subspace contains as much predictive information as the input space. The subspace is usually higher than two but lower than the input space. In contrast, projection-based clustering \code{\link{AutomaticProjectionBasedClustering}} projects the data (nonlinear) into two dimensions and tries only to preerve relevant neighborhoods.
}
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different clusters to be built by the proclus or orclust algorithm.}
\item{DimSubspace}{Numerical number defining the dimensionality in which clusters should be search in in the orclust algorithm, for proclus it is an optional parameter}
\item{Type}{
'Orclus',  subspace clustering based on arbitrarily oriented projected cluster generation [Aggarwal and Yu, 2000]

'ProClus' ProClus algorithm for subspace clustering [Aggarwal/Wolf, 1999] 

'Clique' ProClus algorithm finds subspaces of high-density clusters [Agrawal et al., 1999] and [Agrawal et al., 2005]

'SubClu' SubClu algorithm is a density-connected approach for subspace clustering [Kailing et al.,2004]  

}
\item{PlotIt}{Default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}

\item{OrclusInitialClustersNo}{Only for Orclus algorithm: Initial number of clusters (that are computed in the entire data space) must be greater than k. The number of clusters is iteratively decreased by a factor until the final number of k clusters is reached.}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.

For Subclue: "epsilon" and "minSupport", see \code{\link{DBSCAN}}

For Clique: "xi" (number of intervals for each dimension) and "tau" (Density Threshold), see \code{\link{DBSCAN}}
}
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\note{
JAVA_HOME has to be set for rJava to the ProClus algorithm (in windows set PATH env. variable to .../bin path of Java. The architecture of R and Java have to match. Java automatically downloads the Java version of the browser which may not be installed in the architecture in R. In such a case choose a Java version manually.
}
\references{
[Aggarwal/Wolf et al., 1999]  Aggarwal, C. C., Wolf, J. L., Yu, P. S., Procopiuc, C., & Park, J. S.: Fast algorithms for projected clustering, Proc. ACM SIGMoD Record, Vol. 28, pp. 61-72, ACM, 1999.

[Aggarwal/Yu, 2000]  Aggarwal, C. C., & Yu, P. S.: Finding generalized projected clusters in high dimensional spaces, (Vol. 29), ACM, ISBN: 1581132174, 2000.

[Agrawal et al., 1999]: Rakesh Agrawal, Johannes Gehrke, Dimitrios Gunopulos, and Prabhakar Raghavan: Automatic Subspace Clustering of High Dimensional Data for Data Mining Applications, In Proc. ACM SIGMOD, 1999.

[Agrawal et al., 2005]  Agrawal, R., Gehrke, J., Gunopulos, D., & Raghavan, P.: Automatic subspace clustering of high dimensional data, Data Mining and Knowledge Discovery, Vol. 11(1), pp. 5-33. 2005.

[Kailing et al.,2004]  Kailing, Karin, Hans-Peter Kriegel, and Peer Kroeger: Density-connected subspace clustering for high-dimensional data, Proceedings of the 2004 SIAM international conference on data mining. Society for Industrial and Applied Mathematics, 2004
}

 \examples{
data('Hepta')
out=SubspaceClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
}
\author{Michael Thrun}

\keyword{SubspaceClustering}
\concept{Subspace Clustering}