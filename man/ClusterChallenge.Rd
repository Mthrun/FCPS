\name{ClusterChallenge}
\alias{ClusterChallenge}
\alias{GenerateFundamentalClusteringProblem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates a Fundamental Clustering Challenge based on specific artificial datasets.
}
\description{
Lsun3D and FCPS datasets were introduced in various publications for a specific fixed size. This function generalites them for any sample size.
}
\usage{
ClusterChallenge(Name,SampleSize,


PlotIt=TRUE,PointSize=PointSize,Plotter3D=Plotter3D,\dots)
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
if PlotIt=TRUE: see \code{\link{ClusterPlotMDS}}
}
  \item{Plotter3D}{
if PlotIt=TRUE: see \code{\link{ClusterPlotMDS}}
}
  \item{\dots}{
if PlotIt=TRUE: further arguments for \code{\link{ClusterPlotMDS}}
}
}
\details{
A detailed description of the datasets can be found in [Thrun, 2018]. Lsun was extended to Lsun3D in [Thrun, 2018]. Sampling works by combining Pareto Density Estimation with rejection sampling.
}
\value{
 LIST, with
\item{Name}{[1:SampleSize,1:d] data matrix}
\item{Cls }{[1:SampleSize] numerical vector of classification}

}
\references{
[Thrun/Ultsch, 2020]  Thrun, M. C., & Ultsch, A.: Clustering Benchmark Datasets Exploiting the Fundamental Clustering Problems, Data in Brief, in press, 2020.

\url{https://authors.elsevier.com/tracking/article/details.do?aid=105501&jid=DIB&surname=Thrun}
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
\keyword{GenerateFundamentalClusteringProblem}% use one of  RShowDoc("KEYWORDS")
\keyword{FCPS}% __ONLY ONE__ keyword per line
\keyword{Clustering}%
\keyword{ClusterChallenge}%