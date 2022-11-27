\name{ClusterApply}
\alias{ClusterApply}
\title{Applies a function over grouped data}
\description{
Applies a given function to each dimension \code{d} of data separately for each cluster}
\usage{
ClusterApply(DataOrDistances,FUN,Cls,Simple=FALSE,\ldots)
}
\arguments{
  \item{DataOrDistances}{  [1:n,1:d] with: if d=n and symmetric then distance matrix assumed, otherwise:
  
  [1:n,1:d] matrix of defining the dataset that consists of \code{n} cases or d-dimensional data points. Every case has \code{d} attributes, variables or features.
}
  \item{FUN}{Function to be applied to each cluster of data and each column of data}
  \item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
    \item{Simple}{Boolean, if TRUE, simplifies output}
  \item{\ldots}{Additional parameters to be passed on to FUN}
}
\details{
Applies a given function to each feature of each cluster of data using the clustering stored in \code{Cls} which is the cluster identifiers for all rows in data. If missing, all data are in first cluster, The main output is \code{FUNPerCluster[i]} which is the result of \code{FUN} for the data points in cluster of \code{UniqueClusters[i]} named with the function's name used.

In case of a distance matrix an automatic classical multidimensional scaling transformation of distances to data is computed. Number of dimensions is selected by the minimal stress w.r.t. the possible output dimensions of cmdscale.

If \code{FUN} has not function name, then ResultPerCluster is given back.
}
\value{
if(Simple==FALSE)
List with
\item{UniqueClusters}{The unique clusters in Cls}
\item{FUNPerCluster}{a matrix of [1:k,1:d] of d features and k clusters, the list element is named by the function \code{FUN} used }

if(Simple==TRUE)

a matrix of [1:k,1:d] of d features and k clusters


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