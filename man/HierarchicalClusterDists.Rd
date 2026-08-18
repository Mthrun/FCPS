\name{HierarchicalClusterDists}
\alias{HierarchicalClusterDists}
\title{Internal Hierarchical Clustering from Precomputed Dissimilarities}
\description{
Performs agglomerative hierarchical clustering from a precomputed
dissimilarity object.

The function accepts either a \code{dist} object or a validated square
dissimilarity matrix. If \code{ClusterNo > 0}, the hierarchy is cut into the
requested number of clusters. If \code{ClusterNo = 0}, the complete
dendrogram is plotted and returned.

This is primarily an internal helper. For the usual user interface, see
\code{\link{HierarchicalClustering}}.
}
\usage{
HierarchicalClusterDists(
  pDist,
  ClusterNo = 0,
  Type = "ward.D2",
  ColorTreshold = 0,
  Fast = FALSE,
  ...
)
}
\arguments{
  \item{pDist}{
    Precomputed pairwise dissimilarities supplied either as an object of class
    \code{"dist"} or as a finite, non-negative, symmetric numeric
    \eqn{n \times n} matrix with zero diagonal.

    A raw feature matrix is not converted to distances by this function.
    Distances must be calculated beforehand, for example with
    \code{stats::dist()}.

    For \code{Type = "ward.pseudo"}, \code{pDist} must contain the original
    unsquared dissimilarities.
  }

  \item{ClusterNo}{
    Integer between zero and the number of observations.

    If \code{ClusterNo = 0}, the complete hierarchy is plotted and
    \code{Cls = NULL} is returned.

    If \code{ClusterNo > 0}, the hierarchy is cut into exactly
    \code{ClusterNo} clusters using \code{stats::cutree()}.
  }

  \item{Type}{
    Character scalar specifying the agglomeration method. Supported values are
    \code{"ward.D"}, \code{"ward.D2"}, \code{"ward.pseudo"},
    \code{"single"}, \code{"complete"}, \code{"average"},
    \code{"mcquitty"}, \code{"median"}, and \code{"centroid"}.

    See \code{Details} for the distinction between the three Ward variants.
  }

  \item{ColorTreshold}{
    Finite numeric scalar. Legacy spelling retained for backward compatibility.

    If nonzero and \code{ClusterNo = 0}, the value is interpreted on the
    dendrogram height scale. For monotone fusion heights,
    \code{stats::rect.hclust()} draws rectangles at this height. If fusion
    heights are non-monotone, only a horizontal reference line is drawn.

    Height scales depend on \code{Type}; in particular, a threshold used for
    \code{"ward.D2"} cannot be transferred unchanged to
    \code{"ward.pseudo"}.
  }

  \item{Fast}{
    Logical scalar. If \code{TRUE} and package \pkg{fastcluster} is installed,
    \code{fastcluster::hclust()} is used. Otherwise
    \code{stats::hclust()} is used. If \code{Fast = TRUE} but
    \pkg{fastcluster} is unavailable, a warning is issued.
  }

  \item{\dots}{
    Additional graphical arguments passed to \code{plot.dendrogram()} when
    \code{ClusterNo = 0}.
  }
}
\value{
A list with components:

\item{Cls}{If, ClusterNo>0: [1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering. Otherwise for ClusterNo=0: NULL}

\item{Dendrogram}{
  Object of class \code{"dendrogram"} representing the complete hierarchy.
}

\item{Object}{
  Object of class \code{"hclust"}.

  For \code{Type = "ward.pseudo"}, \code{Object$height} contains the
  successive increases in normalized pseudo within-cluster inertia.
}
}
\details{
\strong{Input validation}

If \code{pDist} is supplied as a matrix or data frame, it is converted to a
matrix and checked to be numeric, square, at least \eqn{2 \times 2}, finite,
symmetric, non-negative within floating-point tolerance, and to have a zero
diagonal. Negligible asymmetry and small negative round-off values are
corrected before conversion to a \code{dist} object.

If a \code{dist} object is supplied directly, its size, expected vector
length, finiteness, and non-negativity are validated.

\strong{Ward variants}

The Ward methods have distinct interpretations:

\itemize{
  \item \code{"ward.D"} applies R's legacy Ward Lance-Williams update directly
  to the supplied dissimilarities. It should not be interpreted as the
  generalized Ward pseudo-inertia criterion for arbitrary Minkowski
  dissimilarities.

  \item \code{"ward.D2"} uses R's Ward.D2 implementation. The supplied
  dissimilarities are squared internally before the Ward update. For arbitrary
  dissimilarities, this can be interpreted as a Ward-like hierarchy based on
  squared dissimilarities. Reported heights are on a square-root,
  distance-like scale.

  \item \code{"ward.pseudo"} implements the dissimilarity-based generalized
  Ward criterion described by Chavent et al. (2018).
}

For \code{"ward.pseudo"}, let \eqn{w_i} denote the observation weights and
\eqn{\mu_C = \sum_{i \in C} w_i} the weight of cluster \eqn{C}. The
dissimilarity-based pseudo-inertia is

\deqn{
I_D(C) =
\frac{
\sum_{i,j \in C} w_i w_j d_{ij}^2
}{
2 \mu_C
}.
}

At every agglomeration step, clusters \eqn{A} and \eqn{B} minimizing

\deqn{
\Delta_D(A,B) =
I_D(A \cup B) - I_D(A) - I_D(B)
}

are merged.

This implementation uses equal normalized weights

\deqn{
w_i = \frac{1}{n}.
}

Therefore the singleton aggregation costs are

\deqn{
\Delta_{ij} = \frac{d_{ij}^2}{2n}.
}

These transformed aggregation costs, not the original dissimilarities, are
passed internally to the \code{"ward.D"} Lance-Williams update together with
\code{members = rep(1/n, n)}.

The generalized pseudo-inertia criterion is defined for potentially
non-Euclidean dissimilarities.

For equal observation weights, \code{"ward.pseudo"} and
\code{"ward.D2"} applied to the same original dissimilarities produce the same
sequence of cluster mergers, apart from possible ordering differences caused
by exact numerical ties. However, their height scales differ according to

\deqn{
h_{\mathrm{ward.pseudo}} =
\frac{h_{\mathrm{ward.D2}}^2}{2n}.
}
allowing for a better visual identification of an appropriate cut in the dendrogramm.

The \code{"ward.pseudo"} option is not the Ward_p algorithm of de Amorim
(2015), which additionally uses cluster-dependent feature weights and
\eqn{L_p} cluster centers.

\strong{Plotting and non-monotone heights}

When \code{ClusterNo = 0}, the dendrogram is plotted. For
\code{"ward.pseudo"}, the y-axis represents the increase in pseudo
within-cluster inertia; for other methods it is the ordinary fusion height.

If \code{ColorTreshold != 0} and fusion heights are monotone,
\code{rect.hclust()} is used. If they are non-monotone, a horizontal red line
is drawn instead and a warning recommends using \code{ClusterNo} rather than a
height cut to obtain a partition.
}
\note{
A raw \eqn{n \times d} feature matrix must not be passed as \code{pDist}.
Compute a dissimilarity object beforehand.

For example, a Minkowski \eqn{p = 6} hierarchy using the generalized
pseudo-inertia criterion can be obtained from
\code{stats::dist(X, method = "minkowski", p = 6)} followed by
\code{Type = "ward.pseudo"}.
}
\references{
Chavent, M., Kuentz-Simonet, V., Labenne, A., and Saracco, J. (2018).
ClustGeo: an R package for hierarchical clustering with spatial constraints.
\emph{Computational Statistics}, \bold{33}, 1799--1822.
doi: 10.1007/s00180-018-0791-1.

Murtagh, F. and Legendre, P. (2014).
Ward's hierarchical agglomerative clustering method:
Which algorithms implement Ward's criterion?
\emph{Journal of Classification}, \bold{31}, 274--295.
doi: 10.1007/s00357-014-9161-z.

de Amorim, R. C. (2015).
Feature relevance in Ward's hierarchical clustering using the Lp norm.
\emph{Journal of Classification}, \bold{32}, 46--62.
doi: 10.1007/s00357-015-9167-1.
}
\examples{
\dontrun{
data("Hepta")

# Standard Ward.D2 clustering
D <- stats::dist(Hepta$Data)

res <- HierarchicalClusterDists(
  D,
  ClusterNo = 7,
  Type = "ward.D2"
)

res$Cls

# Minkowski p = 6 with generalized Ward pseudo-inertia
Xscaled <- scale(Hepta$Data)

D6 <- stats::dist(
  Xscaled,
  method = "minkowski",
  p = 6
)

res_pseudo <- HierarchicalClusterDists(
  D6,
  ClusterNo = 6,
  Type = "ward.pseudo"
)

# Plot the complete hierarchy
HierarchicalClusterDists(
  D,
  ClusterNo = 0,
  Type = "complete"
)
}
}
\author{
Michael Thrun
}
\seealso{
\code{\link{HierarchicalClusterData}},
\code{\link{HierarchicalClustering}},
\code{\link[stats]{hclust}},
\code{\link[stats]{cutree}}
}
\keyword{Hierarchical}
\keyword{Clustering}
\keyword{Distances}
