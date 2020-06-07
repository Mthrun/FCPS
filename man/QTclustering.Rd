\name{QTclustering}
\alias{QTclustering}
\alias{QTClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stochastic QT Clustering
}
\description{
Stochastic quality clustering of [Heyer et al., 1999] with an improved implementation by [Scharl/Leisch, 2006].
}
\usage{
QTclustering(Data,Radius,PlotIt=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}
\item{Radius}{Maximum radius of clusters. If NULL, automatic estimation can be done with [Thrun et al., 2016] if not otherwise set.}
\item{PlotIt}{Default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}
}

\value{
List of
\item{Cls}{[1:n] numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.
Points which cannot be assigned to a cluster will be reported with 0.
}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Heyer et al., 1999]  Heyer, L. J., Kruglyak, S., & Yooseph, S.: Exploring expression data: identification and analysis of coexpressed genes, Genome research, Vol. 9(11), pp. 1106-1115. 1999.

[Scharl/Leisch, 2006]  Scharl, T., & Leisch, F.: The stochastic QT-clust algorithm: evaluation of stability and variance on time-course microarray data, in Rizzi , A. & Vichi, M. (eds.), Proc. Proceedings in Computational Statistics (Compstat), pp. 1015-1022, Physica Verlag, Heidelberg, Germany, 2006.

[Thrun et al., 2016]  Thrun, M. C., Lerch, F., Loetsch, J., & Ultsch, A. : Visualization and 3D Printing of Multivariate Data of Biomarkers, in Skala, V. (Ed.), International Conference in Central Europe on Computer Graphics, Visualization  and Computer Vision (WSCG), Vol. 24, Plzen, 2016. 

[Ultsch, 2005]  Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, In Baier, D. & Werrnecke, K. D. (Eds.), Innovations in classification, data science, and information systems, (Vol. 27, pp. 91-100), Berlin, Germany, Springer, 2005.
}
\author{
Michael Thrun
}

\examples{
data('Hepta')
out=QTclustering(Hepta$Data,Radius=NULL,PlotIt=FALSE)
}

\keyword{QTClustering}

