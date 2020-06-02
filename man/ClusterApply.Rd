\name{ClusterApply}
\alias{ClusterApply}
\title{Applies a function over grouped data}
\description{
Applies a given function to each dimension for each cluster in Cls over the data.}
\usage{
ClusterApply(DataOrDistances,FUN,Cls,\ldots)
}
\arguments{
  \item{DataOrDistances}{ Either a matrix containing data. n Cases as rows, d variables as columns or a nxn distance matrix}
  \item{FUN}{Function to be applied to each cluster of data and each column of data}
  \item{Cls}{The cluster identifiers for all rows in Data. If missing, all data is in first cluster}
  \item{\ldots}{Additional parameters to be passed on to FUN}
}
\details{In case of a distance matrix an automatic classical multidimensional scaling transformation of distances to data is computed. Number of dimensions is selected by the minimal stress w.r.t. the possible output dimensions of cmdscale.

If \code{FUN} has not function name, then ResultPerCluster is given back.
}
\value{
\item{UniqueClusters}{The unique clusters in Cls}
\item{FUNPerCluster}{\code{FUNPerCluster[i]} is the result of func for the data points in cluster \code{UniqueClusters[i]} named with the function name used.}
}
\examples{
##one dataset
data(Hepta)
Data=Hepta$Data
Cls=Hepta$Cls
#mean per cluster
ClusterApply(Data,mean,Cls)

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
#special case of identitiy
\dontrun{
suppressPackageStartupMessages(library('prabclus',quietly = TRUE))
data(tetragonula)
#Generated Specific Distance Matrix
ta <- alleleconvert(strmatrix=as.matrix(tetragonula[1:236,]))
tai <- alleleinit(allelematrix=ta,distance="none")
Distance=alleledist((unbuild.charmatrix(tai$charmatrix,236,13)),236,13)

MDStrans=ClusterApply(Distance,identity)
}
}
\author{Felix Pape}
\note{MT extended the function for distances}
\keyword{ClusterApply}