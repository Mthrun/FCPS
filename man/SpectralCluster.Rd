\name{SpectralCluster}
\alias{SpectralCluster}
\title{ Spectal Clustering }
\arguments{
\item{Data}{(1:n,1:m)Data to be clustered. n Datapoints with m Attributes}
\item{FestgesetzteClustAnz}{Number of different Clusters to build}
\item{kernel}{Kernelmethod, possible options: rbfdot          Radial Basis kernel function "Gaussian" polydot         Polynomial kernel function vanilladot      Linear kernel function tanhdot         Hyperbolic tangent kernel function laplacedot      Laplacian kernel function besseldot       Bessel kernel function anovadot        ANOVA RBF kernel function splinedot       Spline kernel stringdot       String kernel}
\item{kpar}{Kernelparameter: a character string or the list of hyper-parameters (kernel parameters). The default character string "automatic"   uses a heuristic to determine a suitable value for the width parameter of the RBF kernel. "local"       (local scaling) uses a more advanced heuristic and sets a width parameter for every point in the data set. A list can also be used containing the parameters to be used with the kernel function.}
}
\value{cls[1:n]  vector of integer}
\description{
 Clusters the Data into "FestgesetzteClustAnz" different clusters using the Spectal Clustering Method}
\author{
FL 09/2015}
