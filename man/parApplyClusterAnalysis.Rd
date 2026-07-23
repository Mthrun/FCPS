\name{parApplyClusterAnalysis}
\alias{parApplyClusterAnalysis}
\title{Repeated Application of a Clustering Method}
\description{
Repeatedly applies a clustering method to data or distances. Trials can be
executed serially or on a worker cluster from the recommended \pkg{parallel}
package. A single input or a list of inputs can be processed.
It was used for the benchmarking of clustering algorithms in [Thrun, 2018; Thrun/Ultsch, 2020; Thrun, 2021].
}
\usage{
parApplyClusterAnalysis(
  DataOrDistances,
  FUN,
  NumberOfTrials = 1:100,
  ClusterNo = NULL,
  WorkersOrNo,
  SocketType = "PSOCK",
  SetSeed = TRUE,
  ...
)
}
\arguments{
  \item{DataOrDistances}{
    Data, distances, dissimilarities, or a list of such inputs. A list that is
    not a \code{data.frame} is treated as a collection of inputs.
  }

  \item{FUN}{
    A function or character string naming a clustering function. The clustering
    function should return a list with a unique element named \code{Cls} if a
    clustering matrix is required.
  }

  \item{NumberOfTrials}{
    A positive integer count or a vector of positive integer trial identifiers.
    If a single value \code{n} is supplied, the trial identifiers are
    \code{seq_len(n)}. Default is \code{1:100}.
  }

  \item{ClusterNo}{
    Number of clusters supplied to \code{FUN}. For multiple inputs, this may be
    one value or one value per input. \code{NULL}, an empty value, or
    \code{NA} causes the argument to be omitted from the clustering-function
    call.
  }

  \item{WorkersOrNo}{
    Missing, \code{NULL}, a positive integer, or an object inheriting from
    class \code{"cluster"}.

    If missing, the function attempts to use one fewer than the number of
    detected logical CPU cores. \code{NULL} requests serial execution. A
    positive integer creates a new worker cluster. An existing cluster object
    is used without being stopped by this function.
  }

  \item{SocketType}{
    Non-empty character scalar passed to the \code{type} argument of
    \code{parallel::makeCluster()}. Default is \code{"PSOCK"}.
  }

  \item{SetSeed}{
    Logical scalar. If \code{TRUE}, trial \code{i} uses seed
    \code{1000 + i}. If \code{FALSE}, no deterministic trial seed is assigned.
    Default is \code{TRUE}.
  }

  \item{\dots}{
    Further arguments passed to \code{FUN}. Named arguments in \code{\dots}
    may not redefine arguments already supplied by the wrapper.
  }
}
\details{
Default is the number of cores existing minus 1. 

In \pkg{FCPS} default parameters for each clustering algorithm are used automatically if not specified by the user. \code{parApplyClusterAnalysis} expects in \code{FUN} a function of a clustering algorithm which returns a list of objects of which one is named \code{Cls}. If not given the whole output of \code{FUN} is returned with a warning.

\code{Cls} is a [1:N] numerical vector of of numbers 1:k of the k clusters labeling the data points to the clusters.
}
\value{
For a single input, a list with components:

  \item{Cls_Matrix}{
    Matrix containing one clustering vector per trial. Column names identify
    seeds or trial IDs. It is \code{NULL} if at least one result does not have
    exactly one element named \code{Cls}.
  }
  \item{ComputationTime}{
    Named numeric vector containing elapsed computation time in seconds for
    each trial.
  }
  \item{Seeds}{
    Named integer vector containing the seeds used for the trials, or
    \code{NULL} when \code{SetSeed = FALSE}.
  }
  \item{CAobjects}{
    List of complete clustering results. For this generic wrapper, it is
    included when \code{Cls_Matrix} cannot be constructed.
  }


For multiple inputs, a list containing one result of this form per input is
returned. Input names are retained when present.
}

\references{
Thrun, M. C.: Projection-Based Clustering through Self-Organization and Swarm Intelligence, Springer, Heidelberg, ISBN: 978-3658205393, 2018. 

Thrun, M. C., Ultsch, A.: Swarm Intelligence for Self-Organized Clustering, Artificial Intelligence, Vol. 290, pp. 103237, DOI: 10.1016/j.artint.2020.103237, Elsevier, 2021. 

Thrun, M. C.: Distance-Based Clustering Challenges for Unbiased Benchmarking Studies, Nature Scientific Reports, Vol. 11, pp. 18988, DOI: 10.1038/s41598-021-98126-1, 2021. 

}
\author{
Michael Thrun
}
\examples{
simple_clustering <- function(Data, ClusterNo = 2, ...) {
  fit <- stats::kmeans(Data, centers = ClusterNo)
  list(Cls = fit$cluster, Object = fit)
}

DataList=list(
  Dataset1=matrix(rnorm(40, 0), ncol = 2),
  DataSet2=matrix(rnorm(40, 4), ncol = 2)
)

# Serial example
result <- parApplyClusterAnalysis(
  DataOrDistances = DataList,
  FUN = simple_clustering,
  NumberOfTrials = 2,
  ClusterNo = 2,
  WorkersOrNo = NULL
)

result$Cls_Matrix
result$ComputationTime
}
\seealso{
\code{\link[parallel]{clusterApply}}
}
\keyword{clustering}% use one of  RShowDoc("KEYWORDS")
\keyword{parallel cluster analysis}% __ONLY ONE__ keyword per line
\keyword{parApply}% __ONLY ONE__ keyword per line
\keyword{CA}% __ONLY ONE__ keyword per line
\keyword{benchmarking}% __ONLY ONE__ keyword per line