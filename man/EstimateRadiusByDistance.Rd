\name{EstimateRadiusByDistance}
\alias{EstimateRadiusByDistance}

\title{
Estimate Radius By Distance
}
\description{
Published in [Thrun et al, 2016] for the case of automatically estimating the radius of the P-matrix. Can also be used to estimate the radius parameter for distance based clustering algorithms.
}
\usage{
EstimateRadiusByDistance(DistanceMatrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DistanceMatrix}{
[1:n,1:n]  Distance Matrix of n cases
}
}
\details{
 For density-based clustering algorithms like \code{\link{DBscan}} it is not always usefull.
}
\value{
Numerical scalar defining the radius
}
\references{
[Thrun et al., 2016]  Thrun, M. C., Lerch, F., Loetsch, J., & Ultsch, A.: Visualization and 3D Printing of Multivariate Data of Biomarkers, in Skala, V. (Ed.), International Conference in Central Europe on Computer Graphics, Visualization and Computer Vision (WSCG), Vol. 24, pp. 7-16, Plzen, http://wscg.zcu.cz/wscg2016/short/A43-full.pdf, 2016.
}
\author{
Michael Thrun
}
\note{
Symmetric matrix is assumed.
}

\seealso{
\code{\link[GeneralizedUmatrix]{GeneratePmatrix}}
}
\examples{
data('Hepta')
DistanceMatrix=as.matrix(parallelDist::parallelDist(Hepta$Data))
Radius=EstimateRadiusByDistance(DistanceMatrix)
}

\keyword{Radius}% use one of  RShowDoc("KEYWORDS")
\keyword{Clustering}% __ONLY ONE__ keyword per line
