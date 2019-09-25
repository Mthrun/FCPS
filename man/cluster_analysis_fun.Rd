\name{cluster_analysis_fun}
\alias{cluster_analysis_fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal function
}
\description{
Do not use alone, please use \code{\link{parApplyClusterAnalysis}}.
}
\usage{
cluster_analysis_fun(i, fun, DataOrDistance, ClusterNo, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{
...
}
  \item{fun}{
...
}
  \item{DataOrDistance}{
...
}
  \item{ClusterNo}{
...
}
  \item{\dots}{
...
}
}
\details{
...
}
\value{
...
}
\author{
Michael Thrun
}

\seealso{
\code{\link{parApplyClusterAnalysis}}
}
\examples{
\donttest{
data(Hepta)
Distance=as.matrix(parallelDist::parallelDist(Hepta$Data))
out=cluster_analysis_fun(i = 1,fun = APclusterin
g,DataOrDistance = Distance,ClusterNo = 7)
}
}

