\name{PFclustering}
\alias{PFclustering}
\title{Power Fuzzy Clustering }
\description{
Performs Power Fuzzy Clustering [Nguyen et al, 2026]
}
\usage{
PFclustering(
  Data,
  ClusterNo,
  Fuzzifier = 2,
  DistanceMethod = "Euclidean",
  NoIterations = 100,
  PlotIt = FALSE,
  ...
)
}
\arguments{
  \item{Data}{Numeric data set with \eqn{n} observations and \eqn{d} features.}
  \item{ClusterNo}{Integer or \code{NULL}. Number of clusters to search for.}
  \item{Fuzzifier}{Optional, Numeric scalar, strictly greater than 1. Fuzzifier parameter \code{m}. Default is \code{2}.}
  \item{DistanceMethod}{Optional, Character scalar. Distance metric to use. Choices are
  \code{"Minkowski"}, \code{"Mahalanobis"}, or \code{"Euclidean"}.}
  \item{NoIterations}{Optional, , Integer scalar. Maximum number of iterations. Default is \code{100}.}
  \item{PlotIt}{Optional, Logical. If \code{TRUE}, plots the clustering result via \code{ClusterPlotMDS()}.
  Default is \code{FALSE}.}
  \item{\dots}{Optional, Further parameters passed to \code{pfclust::PFC()}, depending on the selected distance.
  These may include, for example:
  \itemize{
    \item \code{q}: distance exponent, must be strictly greater than 0
    \item \code{p}: Minkowski exponent
    \item \code{alpha}, \code{beta}: Mahalanobis parameters
    \item \code{threshold}: convergence tolerance
  }}
}
\value{
A list with components:
\itemize{
  \item \code{Cls}: numeric vector of length \code{nrow(Data)} containing cluster assignments.
  \item \code{Object}: object returned by \code{pfclust::PFC()}.
}
}
\details{
Please see [Nguyen et al, 2026].
}
\references{
P. T. Nguyen, C. Tortora and A. Punzo: \dQuote{Power Fuzzy Clustering: Flexible Distance Metrics and Inclusion of Covariates,}
\emph{IEEE Transactions on Fuzzy Systems}, Vol. 34 (7), pp. 2171--2182,
doi: 10.1109/TFUZZ.2026.3683998, 2026.
}
\examples{
data('Hepta')
\donttest{
out=PFclustering(Hepta$Data,ClusterNo=7,
PlotIt=FALSE,NoIterations=10)
}
}
\author{
Michael Thrun
}