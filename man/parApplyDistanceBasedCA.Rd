\name{parApplyDistanceBasedCA}
\alias{parApplyDistanceBasedCA}
\title{Repeated Application of a Distance-Based Clustering Method}
\description{
Repeatedly applies a distance-based clustering function to one distance or
dissimilarity input, or to a collection of such inputs. Trials may run
serially or on a \pkg{parallel} worker cluster.
}
\usage{
parApplyDistanceBasedCA(
  Distances,
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
  \item{Distances}{
    Distance or dissimilarity representation accepted by \code{FUN}, or a list
    of such inputs.
  }

  \item{FUN}{
    Function or character string naming a distance-based clustering method.
    The result should normally be a list containing a unique element named
    \code{Cls}.
  }

  \item{NumberOfTrials}{
    Positive integer count or vector of positive integer trial identifiers.
    A scalar \code{n} generates \code{seq_len(n)}. Default is \code{1:100}.
  }

  \item{ClusterNo}{
    Number of clusters passed to \code{FUN}. For multiple inputs, this may be
    scalar or provide one value per input. \code{NULL} or \code{NA} omits
    the argument.
  }

  \item{WorkersOrNo}{
    Missing, \code{NULL}, a positive integer worker count, or an existing
    \code{parallel} cluster. If missing, an automatically selected worker
    count is used. \code{NULL} requests serial execution.
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
    Further arguments passed to \code{FUN}.
  }
}
\details{
The distance input is passed preferentially to a formal argument named
\code{DataOrDistances}, \code{Distances}, \code{Distance}, or
\code{Dissimilarities}. If none is present, the first inspectable non-dots
argument is used.

An internally created worker cluster is stopped when the function exits.
An externally supplied cluster remains under the caller's control.
}
\value{
For one input, a list containing:
  \item{Cls_Matrix}{
    Matrix of clustering assignments, one column per trial, or \code{NULL} if
    a complete matrix cannot be constructed.
  }
  \item{ComputationTime}{
    Named numeric vector containing elapsed trial times.
  }
  \item{Seeds}{
    Named integer vector containing trial seeds, or \code{NULL} when
    \code{SetSeed = FALSE}.
  }
  \item{CAobjects}{
    Named list containing the complete clustering results.
  }


For a collection of inputs, returns a named list containing one result per
distance or dissimilarity input.
}
\section{Operating-system considerations}{
The default PSOCK backend is suitable for Windows and is also portable to
macOS and Unix-like systems. Use serial execution during package examples and
most CRAN tests to avoid platform-dependent worker startup behavior.
}
\seealso{
\code{\link{parApplyClusterAnalysis}},
\code{\link{parApplyDataBasedCA}},
\code{\link[parallel]{parLapply}}
}
\examples{
distance_clustering <- function(Distances, ClusterNo = 2) {
  fit <- stats::hclust(Distances)
  cls <- stats::cutree(fit, k = ClusterNo)
  list(Cls = cls, Object = fit)
}

set.seed(3)
Data <- matrix(rnorm(40), ncol = 2)
Distances <- stats::dist(Data)

result <- parApplyDistanceBasedCA(
  Distances = Distances,
  FUN = distance_clustering,
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