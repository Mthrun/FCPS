\name{QTclustering}
\alias{QTclustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stochastic QT Clustering
}
\description{
stochastic quality clustering of [Heyer et al., 1999] with an improved implementation by [Scharl/Leisch, 2006].
}
\usage{
QTclustering(Data,ClusterNo,PlotIt=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.}
\item{Radius}{Maximum radius of clusters.}

\item{ClusterNo}{Optional if Radius is missing: A number k which defines k different Clusters to be build by the algorithm.}
\item{PlotIt}{default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}

\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{QTObject}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Heyer et al., 1999]  Heyer, L. J., Kruglyak, S., & Yooseph, S.: Exploring expression data: identification and analysis of coexpressed genes, Genome research, Vol. 9(11), pp. 1106-1115. 1999.

[Scharl/Leisch, 2006]  Scharl, T., & Leisch, F.: The stochastic QT-clust algorithm: evaluation of stability and variance on time-course microarray data, in Rizzi , A. & Vichi, M. (eds.), Proc. Proceedings in Computational Statistics (Compstat), pp. 1015–1022, Physica Verlag, Heidelberg, Germany, 2006.
}
\author{
Michael Thrun
}

\examples{
data('Hepta')
out=QTclustering(Hepta$Data,ClusterNo=7,PlotIt=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{QTclustering}% use one of  RShowDoc("KEYWORDS")

