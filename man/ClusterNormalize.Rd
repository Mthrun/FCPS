\name{ClusterNormalize}
\alias{ClusterNormalize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cluster Normalize
}
\description{
Values in Cls are consistently recoded to positive consecutive integers
}
\usage{
ClusterNormalize(Cls)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Cls}{[1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.}
}
\details{
For recoding depending on cluster size please see \code{\link{ClusterRenameDescendingSize}}.
}
\value{
The renamed classification. A vector of clusters recoded to positive consecutive integers.
}

\author{
.
}

\seealso{
\code{\link{ClusterRenameDescendingSize}}
}

\examples{
data('Lsun3D')
Cls=Lsun3D$Cls
#not desceending cluster numbers
Cls[Cls==1]=543
Cls[Cls==4]=1

# Now ordered consecutively
ClusterNormalize(Cls)
}

\keyword{ClusterNormalize}
\concept{Consecutive Clustering}
\concept{Cluster Normalize}

