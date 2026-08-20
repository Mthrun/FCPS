\name{ClusterRenameDescendingSize}
\alias{ClusterRenameDescendingSize}
\title{Cluster Rename Descending Size}
\description{
Renames the clusters of a classification in descending order.}
\usage{
ClusterRenameDescendingSize(Cls,

ProvideClusterNames=FALSE)
}
\arguments{
  \item{Cls}{[1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.}
    \item{ProvideClusterNames}{If \code{TRUE}, also returns the new and old cluster labels. If \code{FALSE}, returns only the relabeled classification.}
}
\details{
The return type depends on \code{ProvideClusterNames} for backward compatibility with existing code.

}
\value{
ProvideClusterNames==FALSE:

\item{RenamedCls}{The renamed classification. A vector of cluster labels in which the largest cluster is C1, the second largest is C2, and so forth}

ProvideClusterNames==TRUE: List V with

\item{RenamedCls}{The renamed classification. A vector of cluster labels in which the largest cluster is C1, the second largest is C2, and so forth}

\item{ClusterName}{[1:k,1:2] matrix of k new numbers and prior numbers}
}
\author{Michael Thrun, Alfred Ultsch}

\keyword{ClusterRenameDescendingSize}
\concept{Descending Clustering}
\concept{Rename Descending Cluster Size}

\examples{
data('Lsun3D')
Cls=Lsun3D$Cls
# not descending cluster numbers
Cls[Cls==1]=543
Cls[Cls==4]=1

# Now ordered per cluster size and descending
ClusterRenameDescendingSize(Cls)
}
\seealso{
\code{\link{ClusterNormalize}}
}