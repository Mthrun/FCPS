\name{ClusterAlignLabels}
\alias{ClusterAlignLabels}
\title{Align Cluster Labels to a Reference Clustering}
\description{
Aligns the numeric labels of a candidate clustering to those of a reference
clustering by solving a one-to-one assignment problem that maximizes the total
number of overlapping observations.

Because cluster labels are arbitrary identifiers, two equivalent partitions
may use different numeric labels. This function finds the optimal relabeling of
the candidate clustering without changing observation order.
}
\usage{
ClusterAlignLabels(Cls_reference, Cls_candidate)
}
\arguments{
  \item{Cls_reference}{
    Numeric vector of length \eqn{n} containing the reference cluster labels.
    These labels define the target labeling.
  }

  \item{Cls_candidate}{
    Numeric vector of length \eqn{n} containing the candidate cluster labels
    that should be aligned to \code{Cls_reference}.
  }
}
\value{
A list with components:

\item{Cls_aligned}{
  Numeric vector of length \eqn{n} containing the optimally relabeled
  candidate clustering. Observation order is unchanged.
}

\item{Mapping}{
  Data frame with one row per candidate label and columns
  \code{candidate_label}, \code{reference_label}, and
  \code{matched_observations}.
}

\item{Matches_before}{
  Integer count of observations for which reference and candidate labels were
  already identical before relabeling.
}

\item{Matches_after}{
  Integer count of observations for which the reference label equals the
  aligned candidate label after optimal relabeling.
}

\item{Agreement_before}{
  Numeric proportion of observations with identical labels before alignment.
}

\item{Agreement_after}{
  Numeric proportion of observations with identical labels after alignment.
}

\item{Overlap_matrix}{
  Integer contingency matrix. Rows correspond to candidate labels and columns
  to reference labels. Entry \code{[i,j]} contains the number of observations
  assigned simultaneously to candidate class \code{i} and reference class
  \code{j}.
}
}
\details{
The function first determines the sorted unique labels in the reference and
candidate clusterings. A one-to-one relabeling is only possible if both
clusterings contain the same number of distinct labels.

A contingency matrix is then constructed with \code{table()}. Its rows
represent candidate labels and its columns represent reference labels.

The optimal label permutation is obtained with

\code{clue::solve_LSAP(overlap_matrix, maximum = TRUE)},

which solves the linear sum assignment problem and maximizes the sum of the
selected overlap counts.

The resulting assignment provides exactly one reference label for each
candidate label. Candidate labels are then replaced through this mapping using
\code{match()}, preserving the original observation order.

The returned agreement statistics allow direct comparison of label agreement
before and after alignment.
}

\examples{
\dontrun{
reference <- c(1, 1, 1, 2, 2, 3, 3)
candidate <- c(2, 2, 2, 3, 3, 1, 1)

result <- ClusterAlignLabels(
  Cls_reference = reference,
  Cls_candidate = candidate
)

result$Cls_aligned
result$Mapping
result$Agreement_before
result$Agreement_after
}
}
\seealso{
\code{\link[clue]{solve_LSAP}}
}
\author{
Michael Thrun
}
\keyword{cluster}
\keyword{classification}
