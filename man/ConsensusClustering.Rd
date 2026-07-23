\name{ConsensusClustering}
\alias{ConsensusClustering}
\title{Consensus Clustering}
\description{
Performs consensus clustering using the \pkg{ConsensusClusterPlus} package.
The input may be a data matrix or a symmetric distance matrix.
}
\usage{
ConsensusClustering(
  Data,
  ClusterNo = NULL,
  PlotIt = FALSE,
  PlotConsensus = FALSE,
  ...
)
}
\arguments{
  \item{Data}{
    Data set with n observations and d features
  }

  \item{ClusterNo}{
    Integer or \code{NULL}. Number of clusters to search for. If \code{NULL},
    the complete object returned by \code{ConsensusClusterPlus()} is returned.
  }

  \item{PlotIt}{
    Logical. If \code{TRUE}, plots the final clustering using
    \code{ClusterPlotMDS()}. Default is \code{FALSE}.
  }

  \item{PlotConsensus}{
    FALSE, NULL, "png", "pdf", or "pngBMP".
  }

  \item{\dots}{
    Further arguments passed to
    \code{ConsensusClusterPlus::ConsensusClusterPlus()}.
  }
}
\value{
If \code{ClusterNo} is not \code{NULL}, a list with components:

\item{Cls}{
  Numeric vector of cluster assignments.
}

\item{Object}{
  Complete object returned by
  \code{ConsensusClusterPlus::ConsensusClusterPlus()}.
}

If \code{ClusterNo = NULL}, the complete result from
\code{ConsensusClusterPlus::ConsensusClusterPlus()} is returned directly.

If \pkg{ConsensusClusterPlus} is unavailable, a fallback list is returned with
all observations assigned to cluster 1 and an explanatory message in
\code{Object}.
}

\references{
Wilkerson, D. M. and Hayes, N. D.:
ConsensusClusterPlus: a class discovery tool with confidence assessments and item tracking,
\emph{Bioinformatics},Vol. \bold{26}(12), pp. 1572--1573, 2010.
}
\examples{
# Requires the Bioconductor package ConsensusClusterPlus

# set.seed(1)
# Data <- matrix(rnorm(100), ncol = 5)
#
# result <- ConsensusClustering(
#   Data = Data,
#   ClusterNo = 3,
#   PlotIt = FALSE,
#   reps = 100,
#   clusterAlg = "hc",
#   distance = "euclidean",
#   seed = 123
# )
#
# result$Cls
}
\author{
Michael Thrun
}
\keyword{ConsensusClustering}
\keyword{clustering}