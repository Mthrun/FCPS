\name{ClusterARI}
\alias{ClusterARI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adjusted Rand index
}
\description{
Adjusted Rand index for two clusterings that should be compared to each other. 
This index has expected value zero for independant clusterings and maximum value 1 (for identical clusterings).
}
\usage{
ClusterARI(Cls1, Cls2,Fast=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Cls1}{
1:n numerical vector of numbers defining the classification as the main output of the first clustering  or trial for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
  \item{Cls2}{
1:n numerical vector of numbers defining the classification as the main output of the second clustering algorithm trial for the n cases of data. It has p unique numbers representing the arbitrary labels of the clustering.
}
  \item{Fast}{
  TRUE:uses mclust package which maybe does not integrate all published insights 
  about ARI
  FALSE: uses partitionComparison package
  }
}
\details{
"The expected value of the Rand Index of two random partitions does not take a constant value (e.g. zero). 
Thus, Hubert and Arabie proposed an adjustment [Hubert & Arabie]  which assumes a generalized hypergeometric distribution as null hypothesis: 
the two clusterings are drawn randomly with a fixed number of clusters and a fixed number of elements in each cluster 
(the number of clusters in the two clusterings need not be the same). 
Then the adjusted Rand Index is the (normalized) difference of the Rand Index and its expected value under the null hypothesis. 
The significance of this measure has to be put into question because of the strong assumptions it makes on the distribution. 
Meila [Meila, 2003] notes, that some pairs of clusterings may result in negative index values" [Wagner and Wagner, 2007].
}

\note{
the equation of adjusted random index ignores the labels themselve and measures only the agreement. Hence, one can compare clusterin solutions for k!=p unique numbers that represent the labels, see second example

}
\value{
value of adjusted rand index
}
\references{
[Rand, 1971]  Rand, W. M.: Objective criteria for the evaluation of clustering methods, Journal of the American Statistical Association, Vol. 66(336), pp. 846-850, 1971.

[Hubert & Arabie]  Hubert, L. and Arabie, P.: Comparing partitions, Journal of Classification. Vol. 2 (1), pp. 193-218. doi:10.1007/BF01908075, 1985.

[Ball/Geyer-Schulz, 2018]  Ball, F., & Geyer-Schulz, A.: Invariant Graph Partition Comparison Measures, Symmetry, Vol. 10(10), pp. 1-27, 2018.

[Meila, 2003]	Meila, Marina: Comparing Clusterings. COLT 2003.

[Wagner and Wagner, 2007]	Wagner, Silke; Wagner, Dorothea. Comparing clusterings: an overview. Karlsruhe: Universitaet Karlsruhe, Fakultaet für Informatik, 2007.
}
\author{
Michael Thrun (Wrapper only)
}

\seealso{
\code{\link[partitionComparison]{adjustedRandIndex}}
}
\examples{
data(Hepta)
#compare to baseline
Cls2=kmeansClustering(Hepta$Data,7,Type = "Steinley")$Cls
ClusterARI(Hepta$Cls,Cls2)
#compare different solutions
Cls3=kmeansClustering(Hepta$Data,5)$Cls
ClusterARI(Cls3,Cls2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
 \keyword{ARI}
 \keyword{Rand}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{adjusted rand index}
% \concept{ ~cpt2 }
% Use only one concept per line.
