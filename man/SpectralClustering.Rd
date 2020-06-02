\name{SpectralClustering}
\alias{SpectralClustering}
\title{ Spectral Clustering }

\usage{
SpectralClustering(Data, ClusterNo,PlotIt=FALSE,...)
}
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{PlotIt}{default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.
e.g.:

\code{kernel} : Kernelmethod, possible options: rbfdot          Radial Basis kernel function "Gaussian" polydot         Polynomial kernel function vanilladot      Linear kernel function tanhdot         Hyperbolic tangent kernel function laplacedot      Laplacian kernel function besseldot       Bessel kernel function anovadot        ANOVA RBF kernel function splinedot       Spline kernel stringdot       String kernel

\code{kpar} : Kernelparameter: a character string or the list of hyper-parameters (kernel parameters). The default character string "automatic" uses a heuristic to determine a suitable value for the width parameter of the RBF kernel. "local" (local scaling) uses a more advanced heuristic and sets a width parameter for every point in the data set. A list can also be used containing the parameters to be used with the kernel function.
}
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}



\description{
 Clusters the Data into "ClusterNo" different clusters using the Spectral Clustering method}
 
 \examples{
data('Hepta')
out=SpectralClustering(Hepta$Data,ClusterNo=7,PlotIt=FALSE)
}

\author{Michael Thrun}

\references{
[Ng et al., 2002]  Ng, A. Y., Jordan, M. I., & Weiss, Y.: On spectral clustering: Analysis and an algorithm, Advances in neural information processing systems, Vol. 2, pp. 849-856. 2002.
}
\keyword{SpectralClustering}
\concept{Spectral Clustering}