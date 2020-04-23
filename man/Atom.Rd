\name{Atom}
\alias{Atom}

\docType{data}
\title{
Atom introduced in [Ultsch, 2004].
}
\description{
Two nested spheres with different variances that are not linear not separable.
Detailed description of dataset and its clustering challenge is provided in [Thrun/Ultsch, 2020].
}
\usage{data("Atom")}

\details{
Size 800, Dimensions 3, stored in \code{Atom$Data}

Classes 2, stored in \code{Atom$Cls}
}

\references{
[Ultsch, 2004]  Ultsch, A.: Strategies for an artificial life system to cluster high dimensional data, Abstracting and Synthesizing the Principles of Living Systems, GWAL-6, U. Brggemann, H. Schaub, and F. Detje, Eds, pp. 128-137. 2004.

[Thrun/Ultsch, 2020]  Thrun, M. C., & Ultsch, A.: Clustering Benchmark Datasets Exploiting the Fundamental Clustering Problems, Data in Brief, Vol. in press, pp. 105501, \doi{10.1016/j.dib.2020.105501}, 2020.


}
\examples{
data(Atom)
str(Atom)
}
\keyword{datasets}
\keyword{FCPS}
\keyword{Atom}
