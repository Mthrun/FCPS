\name{Lsun3D}
\alias{Lsun3D}

\docType{data}
\title{
Lsun3D inspired by FCPS
}
\description{
clearly defined clusters, different variances
}
\usage{data("Lsun3D")}

\details{
Size 404, Dimensions 3

Dataset defined discontinuites, where the clusters have different variances.
Three main Clusters, and four Outliers (in Cluster 4). See for a more detailed description in [Thrun, 2018].
}

\references{
[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, Heidelberg, ISBN: 978-3-658-20539-3, \url{https://doi.org/10.1007/978-3-658-20540-9}, 2018. 
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
