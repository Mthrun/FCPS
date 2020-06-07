\name{NeuralGasClustering}
\alias{NeuralGasClustering}
\title{Neural gas algorithm for clustering}

\usage{
NeuralGasClustering(Data, ClusterNo,PlotIt=FALSE,...)
}

\description{
Neural gas clustering published by [Martinetz et al., 1993]] and implemented by [Bodenhofer et al., 2011].

}
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}

 \examples{
data('Hepta')
out=NeuralGasClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
}
\author{Michael Thrun}



\references{
[Dimitriadou, 2002]  Dimitriadou, E.: cclust-convex clustering methods and clustering indexes. R package, 2002,

[Martinetz et al., 1993]  Martinetz, T. M., Berkovich, S. G., & Schulten, K. J.: 'Neural-gas' network for vector quantization and its application to time-series prediction, IEEE Transactions on Neural Networks, Vol. 4(4), pp. 558-569. 1993.
}

\keyword{NeuralGasClustering}% __ONLY ONE__ keyword per line
\concept{Neural Gas}% __ONLY ONE__ keyword per line