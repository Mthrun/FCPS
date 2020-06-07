\name{HCLclustering}
\alias{HCLclustering}
\title{On-line Update (Hard Competitive learning) method}

\usage{
HCLclustering(Data, ClusterNo,PlotIt=FALSE,...)
}
\description{
Hard Competitive learning clustering published by [Ripley, 2007].

}
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different clusters to be build by the algorithm.}
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
out=HCLclustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
}
\author{Michael Thrun}

\references{
[Dimitriadou, 2002]  Dimitriadou, E.: cclust-convex clustering methods and clustering indexes. R package, 2002,

[Ripley, 2007]  Ripley, B. D.: Pattern recognition and neural networks, Cambridge university press, ISBN: 0521717701, 2007.

}

\keyword{HCLclustering}
\concept{Hard Competitive learning clustering}