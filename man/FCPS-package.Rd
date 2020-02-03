\name{FCPS-package}
\alias{FCPS-package}
\alias{ClusteringAlgorithms}

\docType{package}
\title{
FCPC Data Sets with Common Clustering Algorithms
}
\description{
The 'Fundamental Clustering Problems Suite' (FCPS) offers a variety of clustering problems [Ultsch, 2005] any algorithm shall be able to handle when facing real world data. FCPS serves as an elementary benchmark for clustering algorithms.

In the FCPS package 25 clustering algorithms are provided the best ones are used in [Thrun, 2018].

FCPS consists of data sets with known a priori classifications that are to be reproduced by the algorithm. All data sets are intentionally created to be simple and might be visualized in two or three dimensions. Each data sets represents a certain problem that is solved by known clustering algorithms with varying success. This is done in order to reveal benefits and shortcomings of algorithms in question. Standard clustering methods, e.g. single-linkage, ward und k-means, are not able to solve all FCPS problems satisfactorily.
}

\author{
 Authors@R: c(person("Michael", "Thrun", email= "m.thrun@gmx.net",role=c("aut","cre","cph")),person("Peter", "Nahrgang",role=c("ctr","ctb")),person("Alfred", "Ultsch",role=c("dtc","ctb")))

 Maintainer: Michael Thrun <m.thrun@gmx.net>
}
\references{
[Thrun, 2018] Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, ISBN: 978-3-658-20539-3, Heidelberg, 2018.

[Ultsch, 2005] Ultsch, A.: Clustering with SOM: U*C, In Proc. Workshop on Self-Organizing Maps, pp. 75-82, Paris, France, 2005.
}

\keyword{FCPS}
\keyword{data set}
\keyword{benchmarking}
\keyword{clustering}
\keyword{cluster}