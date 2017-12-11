\name{DBscan}
\alias{DBscan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DBscan
}
\description{
DBscan clustering
}
\usage{
DBscan(Data,eps,minPts=5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n,1:d]          der Datensatz 
}
  \item{eps}{
 Radius [Ester et al., 1996, p. 227] (Nachbahrschaft im R-kugel graph/unit disk graph), size of the epsilon neighborhood.
}
  \item{minPts}{
 number of minimum points in the eps region (for core points). 
 In principle minimum number of points in the unit disk, if the unit disk is within the cluster (core) [Ester et al., 1996, p. 228].
 Default is 5 points.
}
}

\value{
Cls[1:n]                Clusterung der Daten, Points which cannot be assigned to a cluster will be reported as members of the noise cluster with NaN.
}
\references{
[Ester et al., 1996]  Ester, M., Kriegel, H.-P., Sander, J., & Xu, X.: A density-based algorithm for discovering clusters in large spatial databases with noise, Proc. Kdd, Vol. 96, pp. 226-231, 1996.

}
\author{
Michael Thrun
}

\examples{
#  res=kmeans(FCPS$Hepta$Data,7)
#  Cls=DBscan(FCPS$Hepta$Data,sqrt(min(res$withinss)))
}
