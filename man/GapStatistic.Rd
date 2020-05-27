\name{GapStatistic}
\alias{GapStatistic}
\title{
Gap Statistic
}
\description{
Gap Statistic
}
\usage{
GapStatistic(Data, ClusterNoMax, ClusterFun, \dots)
}
\arguments{
  \item{Data}{
[1:n,1:d] data matrix
}
  \item{ClusterNoMax}{
max no of clusters to beinvestigated
}
  \item{ClusterFun}{
which clustering algorithm to investigate
}
  \item{\dots}{
further arguments passed on
}
}
\details{
does not work on hepta, see example
}
\value{
tobedocumented
}
\references{
Tibshirani, R., Walther, G. and Hastie, T: Estimating the number of data clusters via the Gap statistic, Journal of the Royal Statistical Society B, Vol. 63, pp. 411-423, 2003.
}
\author{
Michael Thrun
}
\note{
Wrapper only
}
\examples{
data(Hepta)
GapStatistic(Hepta$Data,10,ClusterFun = kmeans)
}
\keyword{Gap}
\concept{Gap Statistic}
