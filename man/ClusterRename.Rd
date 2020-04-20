\name{ClusterRename}
\alias{ClusterRename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Renames Clustering
}
\description{
Renames Clustering such that the names of the numerical vectors are the rownames of DataOrDistances
}
\usage{
ClusterRename(Cls, DataOrDistances)
}

\arguments{
  \item{Cls}{
1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
  \item{DataOrDistances}{
Either nonsymmetric [1:n,1:d] datamatrix of n cases and d features or

symmetric [1:n,1:n] distance matrixe~~
}
}
\details{
if DataOrDistances is missing or if inconsistent length, nothing is done.
}
\value{
Cls[1:n] numerical vector named after the rownames of data
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ClusterRename}% use one of  RShowDoc("KEYWORDS")
