\name{APclustering}
\alias{APclustering}
\title{
Affinity Propagation Clustering
}
\description{
Affinity propagation clustering published by [Frey/Dueck, 2007] and implemented by [Bodenhofer et al., 2011].

}
\usage{
APclustering(DataOrDistances,

InputPreference=NA,ExemplarPreferences=NA,

DistanceMethod="euclidean",

Seed=7568,PlotIt=FALSE,Data,\dots)
}
\arguments{
  \item{DataOrDistances}{
  [1:n,1:d] with: if d=n and symmetric then distance matrix assumed, otherwise:
  [1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.
  In the latter case the Euclidean distances will be calculated.
}
  \item{InputPreference}{
  Default parameter set, see \pkg{apcluster}
}
  \item{ExemplarPreferences}{
  Default parameter set, see \pkg{apcluster}
}
  \item{DistanceMethod}{
  DistanceMethod as in \code{\link{dist}} for  \code{\link[apcluster]{similarities}}.
}

  \item{Seed}{
    Set as integervalue to have reproducible results, see \pkg{apcluster}
}
\item{PlotIt}{Default: FALSE, If TRUE and dataset of [1:n,1:d] dimensions then a plot of the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls} will be generated.}
\item{Data}{[1:n,1:d] data matrix in the case that \code{DataOrDistances} is missing and partial matching does not work.}

\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}

}
\details{
Distancematrix D is converted to similarity matrix S with S=-(D^2).

If data matrix is used, then euclidean similarities are calculated by \code{\link[apcluster]{similarities}} and a specifed distance method.

The AP algorithm decides the k number of clusters.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Frey/Dueck, 2007]	Frey, B. J., & Dueck, D.: Clustering by passing messages between data points, Science, Vol. 315(5814), pp. 972-976, <doi:10.1126/science.1136800>, 2007.

[Bodenhofer et al., 2011]	Bodenhofer, U., Kothmeier, A., & Hochreiter, S.: APCluster: an R package for affinity propagation clustering, Bioinformatics, Vol. 27(17), pp, 2463-2464, 2011.

Further details in
\url{
http://www.bioinf.jku.at/software/apcluster/ 
}
}
\author{
Michael Thrun
}

\examples{
data('Hepta')
res=APclustering(Hepta$Data, PlotIt = FALSE)
}
\seealso{
\pkg{apcluster}
}
\keyword{apcluster}
\concept{Affinity Propagation}
