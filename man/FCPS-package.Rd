\name{FCPS-package}
\alias{FCPS-package}
\alias{ClusteringAlgorithms}

\docType{package}
\title{
FCPC Data Sets with Common Clustering Algorithms
}
\description{
The 'Fundamental Clustering Problems Suite' (FCPS) originally offered a variety of clustering problems
any algorithm shall be able to handle when facing real world data. FCPS served as an elementary benchmark for clustering algorithms.

The FCPS package extends datasets and provides a standardized and easy access to many clustering algorithms.

FCPS datasets consists of data sets with known a priori classifications that are to be reproduced by the algorithm.
 All data sets are intentionally created to be simple and might be visualized in two or three dimensions.
 Each data sets represents a certain problem that is solved by known clustering algorithms with varying success. 
 This is done in order to reveal benefits and shortcomings of algorithms in question. Standard clustering methods,
 e.g. single-linkage, ward and k-means, are not able to solve all FCPS problems satisfactorily.

"Lsun3D and each of the nine artificial data sets of "Fundamental Clustering Problems Suite" (FCPS) 
were defined separately for a specific clustering problem as cited (in [Thrun/Ultsch, 2020]), 
but nine of the here presented artificial datasets were named FCPS in [Ultsch, 2005]. The original 
sample size defined in the respective first publication mentioning the data was used in [Thrun/Ultsch, 2020],
 but using the R function "GenerateFundamentalClusteringProblem" (...) any sample size can be drawn for all artificial data sets. "
[Thrun/Ultsch, 2020]
}

\author{
 Authors@R: c(person("Michael", "Thrun", email= "m.thrun@gmx.net",role=c("aut","cre","cph")),person("Peter", "Nahrgang",role=c("ctr","ctb")),person("Alfred", "Ultsch",role=c("dtc","ctb")))

 Maintainer: Michael Thrun <m.thrun@gmx.net>
}
\references{
[Thrun, 2018] Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, ISBN: 978-3-658-20539-3, Heidelberg, 2018.

[Thrun/Ultsch, 2020]  Thrun, M. C., Ultsch, A.: Clustering Benchmark Datasets Exploiting the Fundamental Clustering Problems, Data in Brief, 2020.

[Ultsch, 2005] Ultsch, A.: Clustering with SOM: U*C, In Proc. Workshop on Self-Organizing Maps, pp. 75-82, Paris, France, 2005.
}

\keyword{FCPS}
\keyword{data set}
\keyword{benchmarking}
\keyword{clustering}
\keyword{cluster}