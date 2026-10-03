\name{BalancedKmeansClustering}
\alias{BalancedKmeansClustering}
\title{Balanced k-Means Clustering Using BKM+}
\description{
Performs balanced k-means clustering using an independent base-R
implementation of Balanced k-Means Revisited (BKM+) [de Maeyer et al., 2023].

The method combines squared-Euclidean k-means optimization with an adaptive
cluster-size penalty. It searches for a partition whose difference between the
largest and smallest cluster sizes does not exceed \code{MaxDiff}, while
retaining a low total within-cluster sum of squares.
}
\usage{
BalancedKmeansClustering(
  Data,
  ClusterNo = 2,
  MaxDiff = 1,
  IterMax = 1000,
  Switch = 10,
  Nstart = 1,
  Centers = NULL,
  Seed = NULL,
  PartlyRemainingFraction = 0.15,
  IncreasingPenaltyFactor = 1.01,
  UseFunctionIter = TRUE,
  StopWhenBalanced = FALSE,
  PlotIt = FALSE,
  Verbose = FALSE,
  KeepHistory = TRUE
)
}
\arguments{
  \item{Data}{
      [1:n,1:d] matrix containing the dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.
  }

  \item{ClusterNo}{
    Positive integer number of clusters. It must lie between one and the number
    of observations.
  }

  \item{MaxDiff}{
    Non-negative integer giving the largest permitted difference between the
    largest and smallest cluster sizes. Default is \code{1}.

    \code{MaxDiff = 0} requests equal cluster sizes and is feasible only when
    \code{ClusterNo} divides the number of observations.
  }

  \item{IterMax}{
    Positive integer maximum number of adaptive-penalty iterations per start.
    Default is \code{1000}.
  }

  \item{Switch}{
    Non-negative integer maximum number of pairwise switch-refinement passes.
    \code{Switch = 0} disables this refinement. Default is \code{10}.
  }

  \item{Nstart}{
    Positive integer number of initializations. Default is \code{1}. If
    \code{Centers} is supplied, \code{Nstart} must equal one.
  }

  \item{Centers}{
    Optional initial centroid matrix with \code{ClusterNo} rows and the same
    number of columns as \code{Data}. Values must be finite and real. Default
    is \code{NULL}.
  }

  \item{Seed}{
    \code{NULL} or one non-negative integer used for random initialization.

    An explicit seed is local to the function call: the previous global
    random-number-generator state is restored on exit, including after errors.
    With \code{Seed = NULL}, initialization advances the ordinary R random
    number stream.
  }

  \item{PartlyRemainingFraction}{
    Finite numeric scalar strictly between zero and one. It supplies the
    fractional term used in the adaptive cluster-size penalty after an
    observation is temporarily removed from its current cluster. Default is
    \code{0.15}.
  }

  \item{IncreasingPenaltyFactor}{
    Finite numeric scalar at least one. Constant multiplier used to increase
    the balancing penalty when \code{UseFunctionIter = FALSE}. Default is
    \code{1.01}.
  }

  \item{UseFunctionIter}{
    Logical scalar. If \code{TRUE}, an iteration-dependent penalty multiplier
    is used. If \code{FALSE}, \code{IncreasingPenaltyFactor} is used.
    Default is \code{TRUE}.
  }

  \item{StopWhenBalanced}{
    Logical scalar. If \code{TRUE}, a feasible size-balanced iteration that no
    longer improves the best feasible SSE may terminate the balancing phase.
    Default is \code{FALSE}.
  }

  \item{PlotIt}{
    Logical scalar. If \code{TRUE}, plots the selected clustering with
    \code{ClusterPlotMDS()}. At least three observations are required.
    Default is \code{FALSE}.
  }

  \item{Verbose}{
    Logical scalar. If \code{TRUE}, reports the SSE, cluster-size difference,
    and termination status for every start. Default is \code{FALSE}.
  }

  \item{KeepHistory}{
    Logical scalar. If \code{TRUE}, retains balancing and switch-refinement
    histories in the returned object. Default is \code{TRUE}.
  }
}
\value{
A list with components:

\item{Cls}{
  Numerical vector of length \eqn{n} containing integer-valued cluster labels
  from \code{1} through \code{ClusterNo}. Element \code{Cls[i]} is the cluster
  assigned to row \code{i} of \code{Data}. The input-row order and the native
  cluster numbering of the selected run are preserved.
}

\item{Object}{
  Detailed BKM+ result. Important fields include \code{Cls}, \code{centers},
  \code{size}, \code{withinss}, \code{tot.withinss}, \code{MSE},
  \code{iter}, \code{best.iter}, \code{penalty}, \code{last.penalty},
  \code{maxdiff}, \code{balanced}, \code{hard.balanced},
  \code{converged}, \code{termination}, \code{initial.centers},
  \code{initial.empty.clusters}, \code{sse.before.switch},
  \code{switch.iter}, \code{switches}, \code{switch.converged},
  \code{history}, \code{switch.history}, \code{nstart},
  \code{best.start}, \code{starts}, \code{maxdiff.requested},
  \code{parameters}, and \code{call}. \code{Object$Cls} is identical to the
  main output \code{Cls}.
}

\item{Centroids}{
  Final centroid matrix, identical to \code{Object$centers}. Row \code{j}
  corresponds to cluster \code{j} in \code{Cls}.
}
}
\details{
\strong{Feature data only}

BKM+ updates arithmetic centroids in the original feature space. In addition to objects of class 
\code{"dist"}, a finite numeric square matrix
is rejected as a distance matrix when it is approximately symmetric, has an
approximately zero diagonal, and has no materially negative entries. A square
feature matrix that also has all of these properties is therefore ambiguous
and is rejected deliberately.

\strong{Initialization}

If \code{Centers = NULL}, \code{ClusterNo} observations are sampled without
replacement as initial centers. Initial assignment uses squared Euclidean
distance and chooses the first cluster in exact ties.

Duplicate observations or supplied centers may create empty initial clusters.
Each empty cluster is initialized with a high-error observation taken from a
non-singleton donor cluster. This repair is confined to initialization.

\strong{Adaptive-penalty balancing}

The algorithm performs sequential observation reassignments. It never removes
an observation from a singleton cluster. Candidate target clusters are
evaluated using squared centroid distance and an adaptive cluster-size
penalty.

A partition is considered balanced when

\deqn{
\max_k n_k - \min_k n_k \leq \mathrm{MaxDiff}.
}

The best feasible assignment is retained according to total within-cluster
SSE. A stored feasible assignment is not discarded if the iteration limit is
subsequently reached.

When \code{UseFunctionIter = TRUE}, the next finite penalty threshold is
multiplied by

\deqn{
1.1009 - 0.0009 s
}

for iteration step \eqn{s \leq 100}, and by \code{1.01} thereafter. When
\code{UseFunctionIter = FALSE}, \code{IncreasingPenaltyFactor} is used.

\strong{Switch refinement}

After the balancing phase, the algorithm considers pairwise exchanges of
observations between clusters. Exchanges are accepted when they reduce total
within-cluster SSE. Because observations are exchanged in pairs, cluster sizes
remain unchanged.

\strong{Multiple starts}

Feasible starts always outrank infeasible starts. Among feasible starts, the
smallest total within-cluster SSE is selected. Among infeasible starts, the
smallest cluster-size difference is selected first, with SSE used as a
tie-breaker.

\strong{Classification and output order}

The classification is represented consistently by \code{Cls}. It is a
numerical vector with labels \code{1:ClusterNo}. No post-fitting sorting or
renumbering by centroid coordinates is applied. Thus \code{Cls[i]} always
refers to \code{Data[i, ]}, and all cluster-indexed outputs use the same native
cluster numbering in contrast to the referenced algorithm in C.

\strong{Detailed object fields}

The returned \code{Object} includes:

\describe{
  \item{\code{Cls}}{Same numerical cluster vector as the main output.}
  \item{\code{centers}}{Final centroid matrix.}
  \item{\code{size}}{Cluster sizes.}
  \item{\code{withinss}}{Within-cluster SSE by cluster.}
  \item{\code{tot.withinss}}{Total within-cluster SSE.}
  \item{\code{MSE}}{\code{tot.withinss / n}.}
  \item{\code{iter}}{Balancing iterations executed.}
  \item{\code{best.iter}}{Iteration associated with the retained assignment.}
  \item{\code{penalty}}{Penalty associated with the retained assignment.}
  \item{\code{last.penalty}}{Final penalty reached by the balancing loop.}
  \item{\code{maxdiff}}{Final maximum cluster-size difference.}
  \item{\code{balanced}}{Whether \code{maxdiff <= MaxDiff}.}
  \item{\code{hard.balanced}}{Whether \code{maxdiff <= 1}.}
  \item{\code{converged}}{Balancing convergence indicator.}
  \item{\code{termination}}{Character termination reason.}
  \item{\code{initial.centers}}{Initial centers of the selected start.}
  \item{\code{initial.empty.clusters}}{Number of repaired empty initial clusters.}
  \item{\code{sse.before.switch}}{SSE before switch refinement.}
  \item{\code{switch.iter}}{Number of switch passes.}
  \item{\code{switches}}{Total number of exchanged observation pairs.}
  \item{\code{switch.converged}}{Switch-refinement convergence indicator.}
  \item{\code{history}}{Balancing history when requested.}
  \item{\code{switch.history}}{Switch-pass history when requested.}
  \item{\code{nstart}}{Number of requested starts.}
  \item{\code{starts}}{Summary data frame for all starts.}
  \item{\code{best.start}}{Index of the selected start.}
  \item{\code{maxdiff.requested}}{Requested maximum size difference.}
  \item{\code{parameters}}{Principal input parameters.}
  \item{\code{call}}{Matched call.}
}
}
\references{
[de Maeyer et al., 2023]  de Maeyer, R., Sieranoja, S., and Franti, P.:Balanced k-means revisited, Applied Computing and Intelligence, 3(2), 145-179. \doi{10.3934/aci.2023008}, 2023
}
\examples{
data('Hepta')

out=BalancedKmeansClustering(Hepta$Data,ClusterNo = 7,PlotIt=FALSE)

\dontrun{
set.seed(1)

Data <- rbind(
  matrix(rnorm(60, mean = 0), ncol = 2),
  matrix(rnorm(60, mean = 5), ncol = 2)
)

result <- BalancedKmeansClustering(
  Data = Data,
  ClusterNo = 2,
  MaxDiff = 1,
  Nstart = 10,
  Seed = 123
)

table(result$Cls)
result$Centroids
result$Object$tot.withinss
}
}
\seealso{
\code{\link{kmeansClustering}}, \code{\link[stats]{kmeans}}
}
\author{
Michael Thrun
}
\keyword{cluster}
\keyword{multivariate}
