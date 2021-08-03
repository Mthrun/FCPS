\name{ClusterUpsampling}
\alias{ClusterUpsampling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cluster Up Sampling using SMOTE
}
\description{
Wrapper for one specific internal function of L. Torgo who implemented there the relevant part of the SMOTE algorithm [Chawla et al., 2002].
}
\usage{
ClusterUpsampling(Cls, Data, MinorityCluster,

Percentage = 200, knn = 5, PlotIt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Cls}{
1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
  \item{Data}{
[1:n,1:d] datamatrix of n cases and d features
}
  \item{MinorityCluster}{
scalar defining the number of the cluster to be upsampeled
}
  \item{Percentage}{
pecentage above 100 of who many samples should be taken
}
  \item{knn}{
k nearest neighbors of SMOTE algorithm}
  \item{PlotIt}{
TRUE: plots the result using   \code{\link{ClusterPlotMDS}}
}
}
\details{
 the number of items \code{m} is defined by the scalar \code{Percentage} and the up sampling is combined with the \code{Data} and the \code{Cls} to  \code{DataExt} and \code{ClsExt} such that the sample is placed thereafter.
}
\value{
List with 
 \item{ClsExt }{1:(n+m) numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{DataExt }{[1:(n+m),1:d] datamatrix of n cases and d features}
.
}
\references{
[Chawla et al., 2002]  Chawla, N. V., Bowyer, K. W., Hall, L. O., & Kegelmeyer, W. P.: SMOTE: synthetic minority over-sampling technique, Journal of artificial intelligence research, Vol. 16, pp. 321-357. 2002.
}
\author{
L. Torgo
}

\examples{
data(Lsun3D)
Data=Lsun3D$Data
Cls=Lsun3D$Cls
table(Cls)

V=ClusterUpsampling(Cls,Data,4,1000)
table(V$ClsExt)
}

\keyword{SMOTE}
\keyword{upsampling}
\keyword{over-sampling}
\concept{up sampling}
\concept{over sampling}
