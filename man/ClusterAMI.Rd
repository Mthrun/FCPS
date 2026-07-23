\name{ClusterAMI}
\alias{ClusterAMI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adjusted Mutual Information [Vinh et al., 2009]
}
\description{
Mutual information measures the amount of information that two clusterings share: the higher the MI, the more information shared, indicating a higher similarity between the two clusterings [Cover and Thomas, 1991].
}
\usage{
ClusterAMI(Cls1, Cls2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Cls1}{
1:n numerical vector of numbers defining the classification as the main output of the first clustering  or trial for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
  \item{Cls2}{
1:n numerical vector of numbers defining the classification as the main output of the second clustering algorithm trial for the n cases of data. It has p unique numbers representing the arbitrary labels of the clustering.
}

}
\details{
Adjusted mutual information ranges from 0 (no similarity between true labels and cluster assignments) to 1 (perfect similarity). It adjusts for chance associations between true labels and cluster assignments.
}


\value{
value of adjusted mutual information
}
\references{
[Vinh et al., 2009] Vinh, N. X.; Epps, J.; Bailey, J.: Information theoretic measures for clusterings comparison, Proceedings of the 26th Annual International Conference on Machine Learning (ICML), doi:10.1145/1553374.1553511, 2009.

[Cover and Thomas, 1991]  Cover, T, and Thomas, J.A.: Elements of Information Theory. Wiley, 1991.
}
\author{
Michael Thrun (Wrapper only)
}

\examples{
data(Hepta)
#compare to baseline
Cls2=kmeansClustering(Hepta$Data,7,Type = "Steinley")$Cls
ClusterAMI(Hepta$Cls,Cls2)
#compare different solutions
Cls3=kmeansClustering(Hepta$Data,5)$Cls
ClusterAMI(Cls3,Cls2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
 \keyword{AMI}
 \keyword{information}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{adjusted mutual information}
\concept{mutual information}

