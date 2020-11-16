\name{ConsensusClustering}
\alias{ConsensusClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Consensus Clustering
}
\description{
Cconsensus clustering (CC) method provides quantitative and visual stability evidence for estimating the number of unsupervised classes in a dataset [Wilkerson/Hayes, 2010].
}
\usage{
ConsensusClustering(DataOrDistances, ClusterNo = NULL,

PlotIt = FALSE, PlotConsensus = FALSE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataOrDistances}{
  [1:n,1:d] with: if d=n and symmetric then distance matrix assumed, otherwise:
  [1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.
  In the latter case the Euclidean distances will be calculated.
}
\item{ClusterNo}{Optional, A number k which defines k different Clusters to be build by the algorithm, or NULL, then provides an internal selection of clusters in the algorithm using \code{PlotConsensus=NULL}}

\item{PlotIt}{Default: FALSE, If TRUE and dataset of [1:n,1:d] dimensions then a plot of the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls} will be generated.}

  \item{PlotConsensus}{
Item tracking plot of \code{\link[ConsensusClusterPlus]{ConsensusClusterPlus}} explained in the details section. \code{NULL} shows the plot, otherwise also
\code{pdf}, \code{png} or \code{pngBMP} for bitmap png can be given back.
}
  \item{\dots}{
please see \code{\link[ConsensusClusterPlus]{ConsensusClusterPlus}} for further details
}
}
\details{
The algorithm begins by subsampling a proportion of items and a proportion of features from a data matrix. Each subsample is then partitioned into up to k groups by a user-specified clustering algorithm: agglomerative hierarchical clustering, k-means or a custom algorithm. This process is repeated for a specified number of repetitions. Pairwise consensus values, defined as ‘the proportion of clustering runs in which two items are [grouped] together’ [Monti et al., 2003], are calculated and stored in a consensus matrix (CM) for each k. Then for each k, a final agglomerative hierarchical consensus clustering using distance of 1−consensus values is completed and pruned to k groups, which are called consensus clusters.

The item tracking plot shows the consensus cluster of items (in columns) at each k (in rows). This allows a user to track an item's cluster assignments across different k, to identify promiscuous items that are suggestive of weak class membership, and to visualize the distribution of cluster sizes across k 
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}

}
\references{
[Wilkerson/Hayes, 2010]  Wilkerson, M. D., & Hayes, D. N.: ConsensusClusterPlus: a class discovery tool with confidence assessments and item tracking, Bioinformatics, Vol. 26(12), pp. 1572-1573. 2010.

[Monti et al., 2003]  Monti, S., Tamayo, P., Mesirov, J., & Golub, T.: Consensus clustering: a resampling-based method for class discovery and visualization of gene expression microarray data, Machine Learning, Vol. 52(1-2), pp. 91-118. 2003.
}
\author{
Michael Thrun
}
\note{
Using data matrix instead of distance matrix sometimes results in strange error in subsidiary package.
Hence this function remains under development.
}

\seealso{
\code{\link[ConsensusClusterPlus]{ConsensusClusterPlus}}
}
\examples{
data(Hepta)
distance=as.matrix(dist(Hepta$Data))
CA=ConsensusClustering(distance,7)
table(CA$Cls,Hepta$Cls)
}

\keyword{ConsensusClustering}

\concept{Consensus Clustering}

