\name{ClusterRenameDescendingSize}
\alias{ClusterRenameDescendingSize}
\title{ ClusterRenameDescendingSize}
\description{
Renames the clusters of a classification in descending order.}
\usage{
ClusterRenameDescendingSize(Cls)
}
\arguments{
  \item{Cls}{1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.}
}
% \details{}
\value{
\item{RenamedCls}{The renamed classification. A vector of clusters, were the largest cluster is C1 and so forth}
}
\author{Michael Thrun, Alfred Ultsch}

\keyword{ClusterRenameDescendingSize}
\keyword{DescendingClustertering}% __ONLY ONE__ keyword per line


\examples{
data('Lsun3D')
Cls=Lsun3D$Cls
#not desceending cluster numbers
Cls[Cls==1]=543
Cls[Cls==4]=1

#now ordered ber cluster size and descending
ClusterRenameDescendingSize(Cls)
}
