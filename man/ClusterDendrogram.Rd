\name{ClusterDendrogram}
\alias{ClusterDendrogram}

\title{
Cluster Dendrogram
}
\description{
Presents a dendrogram of a given tree using a colorsquence for the branches defined from the highest cluster size to the lowest cluster size.
}
\usage{
ClusterDendrogram(TreeOrDendrogram, ClusterNo, 

Colorsequence,main='Name of Algorithm')
}

\arguments{
  \item{TreeOrDendrogram}{
Either object of hcclust defining the tree, third list element of hierarchical cluster algorithms of this package

or

Object of class dendrogram,  second list element of hierarchical cluster algorithms.
}
  \item{ClusterNo}{
k number of clusters for cutree.
}
  \item{Colorsequence}{
[1:k] character vector of colors, per default the colorsquence defined in the \pkg{DataVisualizations} is used
}
\item{main}{Title of plot}
}
\details{
Reqires the package \pkg{dendextend} to work correctly.
}
\value{
In mode invisible:

[1:n]  numerical vector defining the clustering of k clusters; this classification is the main output of the algorithm.
}

\author{
Michael Thrun
}

\seealso{
\code{\link{cutree}}, \code{\link{hclust}}
}
\examples{
data(Lsun3D)
listofh=HierarchicalClustering(Lsun3D$Data,0,'SingleL')
Tree=listofh$Object
#given colors are per default:
#"magenta" "yellow"  "black"   "red"
ClusterDendrogram(Tree, 4,main='Single Linkage Clustering')

listofh=HierarchicalClustering(Lsun3D$Data,4)
ClusterCount(listofh$Cls)
#c1 is magenta, c2 is red, c3 is yellow, c4 is black
#because the order of the cluster sizes is
#c1,c3,c4,c2
}

\keyword{ClusterDendrogram}
\keyword{Dendrogram}
\concept{Cluster Dendrogram}