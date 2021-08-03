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
    \item{ProvideClusterNames}{TRUE: Provides in seperate output new and old k numbers, FALSE: simple output }
}
\details{
Beware: output changes in this function depending on \code{ProvideClusterNames} in order to be congruent to prior code in a large varierity of other packages.

}
\value{
ProvideClusterNames==FALSE:

\item{RenamedCls}{The renamed classification. A vector of clusters, were the largest cluster is C1 and so forth}

ProvideClusterNames==TRUE: List V with

\item{RenamedCls}{The renamed classification. A vector of clusters, were the largest cluster is C1 and so forth}

\item{ClusterName}{[1:k,1:2] matrix of k new numbers and prior numbers}
}
\author{Michael Thrun, Alfred Ultsch}

\keyword{ClusterRenameDescendingSize}
\concept{Descending Clustering}
\concept{Rename Descending Cluster Size}

\examples{
data('Lsun3D')
Cls=Lsun3D$Cls
#not desceending cluster numbers
Cls[Cls==1]=543
Cls[Cls==4]=1

# Now ordered per cluster size and descending
ClusterRenameDescendingSize(Cls)
}
\seealso{
\code{\link{ClusterNormalize}}
}