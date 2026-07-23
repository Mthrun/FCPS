\name{cluster_analysis_fun}
\alias{cluster_analysis_fun}
\alias{CA_data_fun}
\alias{CA_dist_fun}
\title{Internal Workers for Repeated Clustering Analysis}
\description{
Internal worker functions used by the parallel clustering-analysis wrappers.
Each function executes one clustering trial, optionally assigns a deterministic
seed, records elapsed computation time, and extracts a result element named
\code{Cls}.
}
\usage{
cluster_analysis_fun(
  i,
  fun,
  DataOrDistances,
  ClusterNo = NULL,
  SetSeed = TRUE,
  ...
)

CA_data_fun(
  i,
  fun,
  Data,
  ClusterNo = NULL,
  SetSeed = TRUE,
  ...
)

CA_dist_fun(
  i,
  fun,
  Distances,
  ClusterNo = NULL,
  SetSeed = TRUE,
  ...
)
}
\arguments{
  \item{i}{
    Positive integer trial identifier. When \code{SetSeed = TRUE}, the seed is
    \code{1000 + i}.
  }

  \item{fun}{
    Function or character string naming the clustering function.
  }

  \item{DataOrDistances}{
    Generic data, distance, or dissimilarity input passed to \code{fun}.
  }

  \item{Data}{
    Dataset passed to a data-based clustering function.
  }

  \item{Distances}{
    Distance or dissimilarity input passed to a distance-based clustering
    function.
  }

  \item{ClusterNo}{
    Optional number of clusters. If \code{NULL}, it is not included in the
    clustering-function call.
  }

  \item{SetSeed}{
    Logical scalar. If \code{TRUE}, use seed \code{1000 + i}. If
    \code{FALSE}, call \code{set.seed(NULL)} before executing the clustering
    function.
  }

  \item{\dots}{
    Further arguments passed to the clustering function.
  }
}
\details{
The functions inspect the formal arguments of \code{fun} to determine the name
under which the main input is supplied.

\code{cluster_analysis_fun()} recognizes generic data and distance argument
names. \code{CA_data_fun()} prefers \code{Data}, and
\code{CA_dist_fun()} prefers names associated with distances or
dissimilarities.

Named arguments in \code{\dots} may not duplicate arguments supplied by the
worker. If \code{fun} has no \code{\dots} formal argument, unsupported named
arguments are removed before the call.

Only an exactly unique element named \code{Cls} is extracted. Otherwise,
\code{Cls} is returned as \code{NULL}.
}
\value{
A list with components:
  \item{Cls}{
    Clustering vector extracted from the result, or \code{NULL}.
  }
  \item{ComputationTime}{
    Named numeric scalar containing elapsed time in seconds.
  }
  \item{Seed}{
    Integer seed, or \code{NULL} when \code{SetSeed = FALSE}.
  }
  \item{CAs}{
    Complete object returned by the clustering function.
  }

}
\note{
These are internal implementation functions. Users should normally call
\code{\link{parApplyClusterAnalysis}},
\code{\link{parApplyDataBasedCA}}, or
\code{\link{parApplyDistanceBasedCA}}.
}
\keyword{internal}
\keyword{clustering}% use one of  RShowDoc("KEYWORDS")
\keyword{parallel cluster analysis}% __ONLY ONE__ keyword per line
\keyword{parApply}% __ONLY ONE__ keyword per line
\keyword{CA}% __ONLY ONE__ keyword per line
\keyword{benchmarking}% __ONLY ONE__ keyword per line

\references{
Thrun, M. C.: Distance-Based Clustering Challenges for Unbiased Benchmarking Studies, Nature Scientific Reports, Vol. 11, pp. 18988, DOI: 10.1038/s41598-021-98126-1, 2021. 
}
\author{
Michael Thrun
}\seealso{
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
