\name{APclustering}
\alias{APclustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Affinity Propagation clustering
}
\description{
Affinity Propagation clustering published by [Frey/Dueck, 2007] and implemented by [Bodenhofer et al., 2011].

}
\usage{
APclustering(DataOrDistances,

InputPreference=NA,ExemplarPreferences=NA,Seed,PlotIt=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataOrDistances}{
  [1:n,1:d] with: if d=n and symmetric then distance matrix assumed, otherwise:
  [1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.
  In the later case the Euclidean distances will be calculated.
}
  \item{InputPreference}{
  default parameter set, see \code{apcluster}
}
  \item{ExemplarPreferences}{
  default parameter set, see \code{apcluster}
}
  \item{Seed}{
    set as integervalue to have reproducible results, see \code{apcluster}
}
\item{PlotIt}{default: FALSE, If TRUE and dataset of [1:n,1:d] dimensions then a plot of the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls} will be generated.}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}

}
\details{
Distancematrix is converted to similaritiy matrix.
If Data matrix is used, then euclidean similarities are calculated by \code{negDistMat}.

The AP algorithm decides the k number of clusters.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{APobject}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Frey/Dueck, 2007]	Frey, B. J., & Dueck, D.: Clustering by passing messages between data points, Science, Vol. 315(5814), pp. 972-976, <doi:10.1126/science.1136800>, 2007.

[Bodenhofer et al., 2011]	Bodenhofer, U., Kothmeier, A., & Hochreiter, S.: APCluster: an R package for affinity propagation clustering, Bioinformatics, Vol. 27(17), pp, 2463-2464, 2011.

further Details in
\url{
http://www.bioinf.jku.at/software/apcluster 
}
}
\author{
Michael Thrun
}


\examples{
data('Hepta')
res=APclustering(Hepta$Data)
\donttest{
#DataVisualizations::plot3D(Hepta$Data,res$Cls)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
