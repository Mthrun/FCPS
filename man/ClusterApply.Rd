\name{ClusterApply}
\alias{ClusterApply}
\title{Applies a function over grouped data}
\description{
Applies a given function to each data dimension separately within each cluster.}
\usage{
ClusterApply(DataOrDistances,FUN,Cls,Simple=FALSE,\ldots)
}
\arguments{
  \item{DataOrDistances}{  Either a symmetric [1:n,1:n] distance matrix or:

  a [1:n,1:d] matrix containing \code{n} cases of d-dimensional data points. Every case has \code{d} attributes, variables, or features.
}
  \item{FUN}{Function to be applied to each cluster of data and each column of data}
  \item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
    \item{Simple}{Boolean, if TRUE, simplifies output}
  \item{\ldots}{Additional parameters to be passed on to FUN}
}
\details{
Applies \code{FUN} to each feature within each cluster defined by \code{Cls}. If \code{Cls} is missing, all observations are assigned to one cluster. The main output contains the result of \code{FUN} for each cluster and is named after the supplied function.

For a distance matrix, classical multidimensional scaling is used automatically to obtain coordinates. The number of dimensions is selected from the possible \code{cmdscale} output dimensions by minimum stress.

If the name of \code{FUN} cannot be extracted, the component is named \code{ResultPerCluster}.
}
\value{
If \code{Simple = FALSE}, a list with:
\item{UniqueClusters}{The unique cluster labels in \code{Cls}.}
\item{FUNPerCluster}{A [1:k,1:d] matrix with one row per cluster and one column per feature. The component name is derived from the supplied \code{FUN}; for example, \code{meanPerCluster}.}

If \code{Simple = TRUE}, a [1:k,1:d] matrix with one row per cluster and one column per feature.


}
\examples{
##one dataset
data(Hepta)
Data=Hepta$Data
Cls=Hepta$Cls
#mean per cluster
ClusterApply(Data,mean,Cls)

#Simplified
ClusterApply(Data,mean,Cls,Simple=TRUE)

# Mean per cluster of MDS transformation
# Beware, this is not the same!
\donttest{
ClusterApply(as.matrix(dist(Data)),mean,Cls)
}

\dontrun{
Iris=datasets::iris
Distances=as.matrix(Iris[,1:4])
SomeFactors=Iris$Species
V=ClusterCreateClassification(SomeFactors)
Cls=V$Cls
V$ClusterNames
ClusterApply(Distances,mean,Cls)
}
#special case of identity
\dontrun{
suppressPackageStartupMessages(library('prabclus',quietly = TRUE))
data(tetragonula)
#Generated Specific Distance Matrix
ta <- alleleconvert(strmatrix=as.matrix(tetragonula[1:236,]))
tai <- alleleinit(allelematrix=ta,distance="none")
Distance=alleledist((unbuild.charmatrix(tai$charmatrix,236,13)),236,13)

MDStrans=ClusterApply(Distance,identity)$identityPerCluster
}
}
\author{Felix Pape, Michael Thrun}
\keyword{ClusterApply}