\name{FCPS-package}
\alias{FCPS-package}
\alias{ClusteringAlgorithms}

\docType{package}
\title{
\packageTitle{FCPS}
}
\description{

\packageDescription{FCPS}

The 'Fundamental Clustering Problems Suite' (FCPS) originally offered a variety of clustering problems
any algorithm shall be able to handle when facing real world data.  Nine of the here presented artificial datasets were priorly named FCPS in Ultsch, A.: "Clustering with SOM: U*C", In Workshop on Self-Organizing Maps, 2005. FCPS often served in the paper as an elementary benchmark for clustering algorithms. The FCPS package extends datasets and provides a standardized and easy access to many clustering algorithms.
}

\details{
FCPS datasets consists of data sets with known a priori classification to be reproduced by the algorithms.
 All data sets are intentionally created to be simple and might be visualized in two or three dimensions.
 Each data sets represents a certain problem that is solved by known clustering algorithms with varying success. 
 This is done in order to reveal benefits and shortcomings of algorithms in question. Standard clustering methods,
 e.g. single-linkage, ward and k-means, are not able to solve all FCPS problems satisfactorily. "Lsun3D and each of the nine artificial data sets of "Fundamental Clustering Problems Suite" (FCPS) 
were defined separately for a specific clustering problem as cited (in [Thrun/Ultsch, 2020]). The original 
sample size defined in the respective first publication mentioning the data was used in [Thrun/Ultsch, 2020],
 but using the R function "ClusterChallenge" (...) any sample size can be drawn for all artificial data sets.
[Thrun/Ultsch, 2020]

\packageIndices{DatabionicSwarm}
}


\author{
\packageAuthor{FCPS}

Maintainer: \packageMaintainer{FCPS}
}
\references{
[Thrun, 2018] Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, ISBN: 978-3-658-20539-3, Heidelberg, 2018.

[Thrun/Ultsch, 2020]  Thrun, M. C., & Ultsch, A.: Clustering Benchmark Datasets Exploiting the Fundamental Clustering Problems, Data in Brief, Vol. in press, pp. 105501, \doi{10.1016/j.dib.2020.105501}, 2020.

[Ultsch, 2005] Ultsch, A.: Clustering with SOM: U*C, In Proc. Workshop on Self-Organizing Maps, pp. 75-82, Paris, France, 2005.
}

\keyword{FCPS}
\concept{data set}
\keyword{benchmarking}
\keyword{clustering}
\keyword{cluster}
\concept{Fundamental Clustering Problems Suite}
