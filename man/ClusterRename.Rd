\name{ClusterRename}
\alias{ClusterRename}

\title{
Renames Clustering
}
\description{
Renames Clustering such that the names of the numerical vectors are the row names of DataOrDistances
}
\usage{
ClusterRename(Cls, DataOrDistances)
}

\arguments{
  \item{Cls}{
1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
  \item{DataOrDistances}{
Either nonsymmetric [1:n,1:d] datamatrix of n cases and d features or symmetric [1:n,1:n] distance matrix
}
}
\details{
If DataOrDistances is missing or if inconsistent length, nothing is done.
}
\value{
Cls[1:n] numerical vector named after the row names of data
}

\author{
Michael Thrun
}

\examples{
data('Hepta')
Cls=Hepta$Cls
Data=Hepta$Data#
#prior
Cls
#Named Clustering
ClusterRename(Cls)
}

\keyword{ClusterRename}% use one of  RShowDoc("KEYWORDS")
