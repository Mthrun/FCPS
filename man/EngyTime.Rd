\name{EngyTime}
\alias{EngyTime}

\docType{data}
\title{
EngyTime introduced in [Baggenstoss, 2002].
}
\description{
Gaussian mixture. Detailed description of dataset and its clustering challenge is provided in [Thrun/Ultsch, 2020].
}
\usage{data("EngyTime")}

\details{
Size 4096, Dimensions 2, stored in \code{EngyTime$Data}

Classes 2, stored in \code{EngyTime$Cls}
}

\references{
[Baggenstoss, 2002]  Baggenstoss, P. M.: Statistical modeling using gaussian mixtures and hmms with matlab, Naval Undersea Warfare Center, Newport RI, 2002.

[Thrun/Ultsch, 2020]  Thrun, M. C., & Ultsch, A.: Clustering Benchmark Datasets Exploiting the Fundamental Clustering Problems, Data in Brief, Vol. 30(C), pp. 105501, \doi{10.1016/j.dib.2020.105501}, 2020.

}
\examples{
data(EngyTime)
str(EngyTime)
}
\keyword{EngyTime}
\keyword{datasets}
\keyword{FCPS}