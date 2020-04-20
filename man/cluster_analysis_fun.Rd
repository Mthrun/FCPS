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
cluster_analysis_fun(i, fun, DataOrDistances, ClusterNo,SetSeed=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{
...
}
  \item{fun}{
...
}
  \item{DataOrDistances}{
...
}
  \item{ClusterNo}{
...
}
  \item{SetSeed}{
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
\dontrun{
#data(Hepta)
#Distance=as.matrix(parallelDist::parallelDist(Hepta$Data))
#out=cluster_analysis_fun(i = 1,fun = APclusterin
#g,DataOrDistances = Distance,ClusterNo = 7)
}
}

