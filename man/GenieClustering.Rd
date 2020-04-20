\name{GenieClustering}
\alias{GenieClustering}
\title{
Genie Clustering by Gini Index
}
\description{
Outlier Resistant Hierarchical Clustering Algorithm of [Gagolewski/Bartoszuk, 2016].
}
\usage{
GenieClustering(DataOrDistances, ClusterNo = 0,
DistanceMethod="euclidean", ColorTreshold = 0,\dots)
}
\arguments{
\item{DataOrDistances}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features. Alternatively, symmetric [1:n,1:n] distance matrix}

\item{ClusterNo}{A number k which defines k different Clusters to be build by the algorithm.}

\item{DistanceMethod}{see  \code{\link[parallelDist]{parDist}}, for example 'euclidean','mahalanobis','manhatten' (cityblock),'fJaccard','binary', 'canberra', 'maximum'. Any unambiguous substring can be given.}

\item{ColorTreshold}{draws cutline w.r.t. dendogram y-axis (height), height of line as scalar should be given}
\item{\dots}{furter argument to genie like:

\code{thresholdGini}  single numeric value in [0,1], threshold for the Gini index, 1 gives the standard single linkage algorithm
}
}
\details{
Wrapper for Genie algorithm.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Dendrogram}{Dendrogram of hclust}
}
\references{
[Gagolewski/Bartoszuk, 2016]  Gagolewski M., Bartoszuk M., Cena A., Genie: A new, fast, and outlier-resistant hierarchical clustering
algorithm, Information Sciences, Vol. 363, pp. 8-23, 2016.
}
\author{
Michael Thrun
}

\seealso{
\code{\link{HierarchicalClustering}}
}
 \examples{
data('Hepta')
out=GenieClustering(Hepta$Data,ClusterNo=7)
}
\keyword{Hierarchical}% use one of  RShowDoc("KEYWORDS")
\keyword{Clustering}% __ONLY ONE__ keyword per line
\keyword{Agglomerative}% __ONLY ONE__ keyword per line
