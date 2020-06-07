\name{ClusterChallenge}
\alias{ClusterChallenge}

\title{
Generates a Fundamental Clustering Challenge based on specific artificial datasets.
}
\description{
Lsun3D and FCPS datasets were introduced in various publications for a specific fixed size. This function generalizes them for any sample size.
}
\usage{
ClusterChallenge(Name,SampleSize,

PlotIt=FALSE,PointSize=1,Plotter3D="rgl",\dots)
}
\arguments{
  \item{Name}{
string, either 'Atom', 'Chainlink, 'EngyTime', 'GolfBall', 'Hepta', 'Lsun3D',  'Target'
    'Tetra'
    'TwoDiamonds'
    'WingNut
}
  \item{SampleSize}{
Size of Sample higher than 300, preferable above 500
}
  \item{PlotIt}{
TRUE: Plots the challenge with \code{\link{ClusterPlotMDS}}
}
  \item{PointSize}{
If PlotIt=TRUE: see \code{\link{ClusterPlotMDS}}
}
  \item{Plotter3D}{
If PlotIt=TRUE: see \code{\link{ClusterPlotMDS}}
}
  \item{\dots}{
If PlotIt=TRUE: further arguments for \code{\link{ClusterPlotMDS}}
}
}
\details{
A detailed description of the datasets can be found in [Thrun/Ultsch 2020]. Sampling works by combining Pareto Density Estimation with rejection sampling.
}
\value{
 LIST, with
\item{Name}{[1:SampleSize,1:d] data matrix}
\item{Cls }{[1:SampleSize] numerical vector of classification}

}
\references{
[Thrun/Ultsch, 2020]  Thrun, M. C., & Ultsch, A.: Clustering Benchmark Datasets Exploiting the Fundamental Clustering Problems, Data in Brief, Vol. in press, pp. 105501, \doi{10.1016/j.dib.2020.105501}, 2020.
}
\author{
Michael Thrun
}

\examples{
\dontshow{
ClusterChallenge("Chainlink",10,FALSE)
}
\donttest{
ClusterChallenge("Chainlink",2000,TRUE)
}
}
\seealso{
\code{\link{ClusterPlotMDS}}
}
\concept{Generate Fundamental Clustering Problem}
\keyword{FCPS}% __ONLY ONE__ keyword per line
\keyword{Clustering}%
\concept{Cluster Challenge}%