\name{GenerateFundamentalClusteringProblem}
\alias{GenerateFundamentalClusteringProblem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates a Fundamental Clustering Problem based on specific artificial datasets.
}
\description{
Lsun3D and FCPS datasets were introduced in various publications for a specific fixed size. This function generalites them for any sample size.
}
\usage{
GenerateFundamentalClusteringProblem(Name,SampleSize,PlotIt=TRUE)
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
TRUE: Plots the Problem
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
[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, Springer, Heidelberg, ISBN: 978-3-658-20539-3, \url{https://doi.org/10.1007/978-3-658-20540-9}, 2018. 

}
\author{
Michael Thrun
}

\examples{
\dontshow{
GenerateFundamentalClusteringProblem("Chainlink",10,FALSE)
}
\donttest{
GenerateFundamentalClusteringProblem("Chainlink",2000,TRUE)
}
}
\keyword{GenerateFundamentalClusteringProblem}% use one of  RShowDoc("KEYWORDS")
\keyword{FCPS}% __ONLY ONE__ keyword per line
\keyword{Clustering}%