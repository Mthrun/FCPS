\name{parApplyDataBasedCA}
\alias{parApplyDataBasedCA}
\title{Repeated Application of a Data-Based Clustering Method}
\description{
Repeatedly applies a data-based clustering method to one dataset or a
collection of datasets. Trials may be executed serially or in parallel.
}
\usage{
parApplyDataBasedCA(
  Data,
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
  \item{Data}{
    Dataset accepted by \code{FUN}, or a list of datasets. A list that is not
    a \code{data.frame} is interpreted as a collection of inputs.
  }

  \item{FUN}{
    Function or character string naming a data-based clustering method. The
    result should normally be a list containing a unique element named
    \code{Cls}.
  }

  \item{NumberOfTrials}{
    Positive integer count or vector of positive integer trial identifiers.
    A scalar \code{n} generates \code{seq_len(n)}. Default is \code{1:100}.
  }

  \item{ClusterNo}{
    Number of clusters passed to \code{FUN}. For multiple datasets, it may be
    scalar or provide one value per dataset. \code{NULL} or \code{NA} omits
    the argument.
  }

  \item{WorkersOrNo}{
    Missing, \code{NULL}, a positive integer worker count, or an existing
    \code{parallel} cluster. If missing, one fewer than the detected logical
    cores is used when possible. \code{NULL} requests serial execution.
  }

  \item{SocketType}{
    Character scalar passed to \code{parallel::makeCluster()}. Default is
    \code{"PSOCK"}.
  }

  \item{SetSeed}{
    Logical scalar. If \code{TRUE}, trial \code{i} uses seed
    \code{1000 + i}. Default is \code{TRUE}.
  }

  \item{\dots}{
    Further arguments passed to \code{FUN}. Wrapper-supplied arguments may not
    be redefined.
  }
}
\details{
The dataset is passed to a formal argument named \code{Data} when available.
Otherwise, \code{DataOrDistances} or the first inspectable non-dots argument
is used.

For multiple datasets, errors raised while processing an individual dataset
are converted to warnings. The corresponding list element is returned as
\code{NULL}, while processing continues for the remaining datasets.

An internally created worker cluster is stopped automatically. A cluster
supplied through \code{WorkersOrNo} is not stopped.
}
\value{
For one dataset, a list with:
  \item{Cls_Matrix}{
    Matrix of clustering vectors, one column per trial, or \code{NULL} if it
    cannot be constructed.
  }
  \item{ComputationTime}{
    Named numeric vector of elapsed trial times.
  }
  \item{Seeds}{
    Named integer vector of seeds, or \code{NULL} if
    \code{SetSeed = FALSE}.
  }
  \item{CAobjects}{
    Named list containing the complete clustering result from every trial.
}


For a collection of datasets, returns a list with one result per dataset.
Failed datasets are represented by \code{NULL}.
}
\section{Operating-system considerations}{
The default \code{"PSOCK"} cluster type is portable across the operating
systems supported by R. Objects required by \code{FUN} must be available or
serializable on each worker.

Set \code{WorkersOrNo = NULL} for deterministic serial testing.
}
\seealso{
\code{\link{parApplyClusterAnalysis}},
\code{\link{parApplyDistanceBasedCA}},
\code{\link[parallel]{makeCluster}}
}
\examples{
simple_clustering <- function(Data, ClusterNo = 2) {
  fit <- stats::kmeans(Data, centers = ClusterNo)
  list(Cls = fit$cluster, Object = fit)
}

set.seed(2)
Data <- matrix(rnorm(80), ncol = 2)

result <- parApplyDataBasedCA(
  Data = Data,
  FUN = simple_clustering,
  NumberOfTrials = 2,
  ClusterNo = 2,
  WorkersOrNo = NULL
)

result$Cls_Matrix
}
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
}