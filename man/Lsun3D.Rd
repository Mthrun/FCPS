\name{Lsun3D}
\alias{Lsun3D}

\docType{data}
\title{
Lsun3D inspired by FCPS introduced in [Thrun, 2018]
}
\description{
Clearly defined clusters, different variances.  Detailed description of dataset and its clustering challenge is provided in [Thrun/Ultsch, 2020].
}
\usage{data("Lsun3D")}

\details{
Size 404, Dimensions 3

Dataset defines discontinuites, where the clusters have different variances.
Three main clusters, and four outliers (in cluster 4). For a more detailed description see [Thrun, 2018].
}

\references{
[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, Heidelberg, ISBN: 978-3-658-20539-3, \doi{10.1007/978-3-658-20540-9}, 2018. 

[Thrun/Ultsch, 2020]  Thrun, M. C., & Ultsch, A.: Clustering Benchmark Datasets Exploiting the Fundamental Clustering Problems, Data in Brief, Vol. 30(C), pp. 105501, \doi{10.1016/j.dib.2020.105501}, 2020.
}
\examples{
data(Lsun3D)
str(Lsun3D)
Cls=Lsun3D$Cls
Data=Lsun3D$Data
}
\keyword{datasets}
\keyword{Lsun3D}
\keyword{FCPS}
