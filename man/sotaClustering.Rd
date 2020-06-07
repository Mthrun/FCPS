\name{SOTAclustering}
\alias{SOTAclustering}
\alias{sotaClustering}
\title{SOTA Clustering}
\description{
Self-organizing Tree Algorithm (SOTA) introduced by [Herrero et al., 2001].
}
\usage{
SOTAclustering(Data, ClusterNo,PlotIt=FALSE,UnrestGrowth,...)
}

\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{PlotIt}{Default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{UnrestGrowth}{
TRUE: forces the \code{ClusterNo} option to uphold.
FALSE: enables the algorithm to find its own number of clusters, in this cases ClusterNo should contain a high number because it is internally set as the number of iterations which is either reached or the max diversity criteria is satisfied priorly.
}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{sotaObject}{Object defined by clustering algorithm as the other output of this algorithm}
}

 \examples{
#Does Work
data('Hepta')
out=SOTAclustering(Hepta$Data,ClusterNo=7)
table(Hepta$Cls,out$Cls)

\donttest{
#Does not work well
data('Lsun3D')
out=SOTAclustering(Lsun3D$Data,ClusterNo=100,PlotIt=FALSE,UnrestGrowth=FALSE)
}
}
\author{Luis Winckelmann*, Vasyl Pihur, Guy Brock, Susmita Datta, Somnath Datta}

 \note{
 *Luis Winckelman intergrated several function from clValid because it's ORPHANED.
 }
\references{
[Herrero et al., 2001]  Herrero, J., Valencia, A., & Dopazo, J.: A hierarchical unsupervised growing neural network for clustering gene expression patterns, Bioinformatics, Vol. 17(2), pp. 126-136. 2001.

}
\keyword{SOTAclustering}
\concept{Self-organizing Tree Algorithm}