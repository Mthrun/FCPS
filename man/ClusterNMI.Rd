\name{ClusterNMI}
\alias{ClusterNMI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalized Mutual Information [Strehl et al., 2003]
}
\description{
Mutual information measures the amount of information that two clusterings share: the higher the MI, the more information shared, indicating a higher similarity between the two clusterings [Cover and Thomas, 1991].
}
\usage{
ClusterNMI(Cls1, Cls2,Variant="max")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Cls1}{
1:n numerical vector of numbers defining the classification as the main output of the first clustering  or trial for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
\item{Cls2}{
1:n numerical vector of numbers defining the classification as the main output of the second clustering algorithm trial for the n cases of data. It has p unique numbers representing the arbitrary labels of the clustering.
}
\item{Variant}{
Optional, string, default "max", alternatives are ifferent variants of NMI: ("min", "sqrt", "sum", "joint")
}
}
\details{
Normalized mutual information scales the result to a fixed range [0, 1], where 0 indicates no mutual information (completely independent clusterings) and 1 indicates perfect correlation (the clusterings are identical). It is normalized by the entropy of both the true labels and the cluster assignments, which makes it more interpretable and less sensitive to the number of clusters.
NMI is often used when you have prior knowledge about the number of clusters or when comparing different clustering results.
}


\value{
value of normalized mutual information
}
\references{
[Strehl et al., 2003] Strehl, Alexander; Ghosh, Joydeep: Cluster Ensembles – A Knowledge Reuse Framework for Combining Multiple Partitions, The Journal of Machine Learning Research, Vol. 3, pp. 583–617. doi:10.1162/153244303321897735, 2003

[Cover and Thomas, 1991]  Cover, T, and Thomas, J.A.: Elements of Information Theory. Wiley, 1991.
}
\author{
Michael Thrun (Wrapper only)
}

\examples{
data(Hepta)
#compare to baseline
Cls2=kmeansClustering(Hepta$Data,7,Type = "Steinley")$Cls
ClusterNMI(Hepta$Cls,Cls2)
#compare different solutions
Cls3=kmeansClustering(Hepta$Data,5)$Cls
ClusterNMI(Cls3,Cls2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{NMI}
\keyword{information}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{normalized mutual information}
\concept{mutual information}
