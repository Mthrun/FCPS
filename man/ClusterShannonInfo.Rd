\name{ClusterShannonInfo}
\alias{ClusterShannonInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Shannon Information
}
\description{
Shannon Information [Shannon, 1948] for each column in ClsMatrix.
}
\usage{
ClusterShannonInfo(ClsMatrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ClsMatrix}{
[1:n,1:C] matrix of C clusterings each columns is defined as: 

1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
}
\details{
Info[1:d]  = sum(-p * log(p)/MaxInfo) for all unique cases with probability p in ClsMatrix[,c] for a column with k clusters  MaxInfo = -(1/k)*log(1/k)
}
\value{
\item{Info}{
 [1:max.nc,1:C] matrix of Shannin informaton as defined in details, each column represents one \code{Cls} of \code{ClsMatrix},each row yields the information of one cluster up the \code{ClusterNo} k, if k<max.nc (highest number of clusters) then NaN are filled.
}
\item{ClusterNo}{
Number of Clusters k found for each \code{Cls}  respectively
}
\item{MaxInfo}{
max per column of \code{Info}
}
\item{MinInfo}{
min per column of \code{Info}
}
\item{MedianInfo}{
median per column of \code{Info}
}
\item{MeanInfo}{
mean per column of \code{Info}
}
}
\references{
[Shannon, 1948]  Shannon, C. E.: A Mathematical Theory of Communication, Bell System Technical Journal, Vol. 27(3), pp. 379-423. doi doi:10.1002/j.1538-7305.1948.tb01338.x, 1948.
}
\author{
Michael Thrun
}
\note{
reeimplemented from Alfred's Ultsch Matlab version but not verified yet.
}
\examples{
# Reading the iris dataset from the standard R-Package datasets
data <- as.matrix(iris[,1:4])
max.nc = 7
# Creating the clusterings for the data set
#(here with method complete) for the number of classes 2 to 8
hc <- hclust(dist(data), method = "complete")
clsm <- matrix(data = 0, nrow = dim(data)[1],

ncol = max.nc)
for (i in 2:(max.nc+1)) {
  clsm[,i-1] <- cutree(hc,i)
}

ClusterShannonInfo(clsm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{Shannon}
\keyword{information}

\concept{Shannon information}
