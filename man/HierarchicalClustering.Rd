\name{HierarchicalClustering}
\alias{HierarchicalClustering}
\title{Unified Wrapper for Hierarchical Clustering}
\description{
Provides a unified interface for hierarchical clustering from either raw
numeric data or precomputed distances.

The wrapper dispatches to the appropriate lower-level implementation according
to the selected clustering \code{Type} and the form of
\code{DataOrDistances}. Ordinary agglomerative methods, generalized Ward
pseudo-inertia clustering, and several specialized hierarchical methods are
supported.
}
\usage{
HierarchicalClustering(
  DataOrDistances,
  ClusterNo = 0,
  Type = "SingleL",
  Fast = TRUE,
  PlotIt=FALSE,
  Data,
  ...
)
}
\arguments{
\item{DataOrDistances}{
Either nonsymmetric [1:n,1:d] numerical matrix of a dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.

or

symmetric [1:n,1:n] distance matrix, e.g. \code{as.matrix(dist(Data,method))}

}


  \item{ClusterNo}{
    Number of clusters.

    For the ordinary hierarchical methods and \code{"WardPseudo"},
    \code{ClusterNo = 0} delegates complete-hierarchy/dendrogram handling to the
    lower-level function. A positive value cuts the hierarchy into the
    requested number of groups.

    For \code{"HDBSCAN"}, values greater than one explicitly cut the returned
    hierarchy, while smaller values use the automatic clustering produced by
    the HDBSCAN implementation.
  }

  \item{Type}{
    Character scalar selecting the hierarchical clustering method.

    The following public choices are supported:

    \itemize{
      \item \code{"Ward"}: Ward hierarchical clustering using the Ward.D2
      criterion. This is the historical/default Ward interpretation in the
      wrapper.

      \item \code{"WardPseudo"}: generalized Ward-like clustering based on
      dissimilarity pseudo-inertia. It is intended for precomputed,
      potentially non-Euclidean dissimilarities, for example Minkowski
      \eqn{p = 6} distances. The input must be the original unsquared
      dissimilarities.

      \item \code{"SingleL"}: single-linkage clustering.

      \item \code{"CompleteL"}: complete-linkage clustering.

      \item \code{"AverageL"}: average-linkage clustering (UPGMA).

      \item \code{"WPGMA"}: weighted pair-group method using arithmetic
      averages.

      \item \code{"MedianL"}: median linkage (WPGMC).

      \item \code{"CentroidL"}: centroid linkage (UPGMC).

      \item \code{"Minimax"}: minimax-linkage clustering through
      \code{MinimaxLinkageClustering()}.

      \item \code{"MinEnergy"}: minimum-energy hierarchical clustering through
      \code{MinimalEnergyClustering()}.

      \item \code{"Gini"}: Genie/Gini-based clustering through
      \code{GenieClustering()}.

      \item \code{"Sparse"}: sparse hierarchical clustering through
      \code{SparseClustering(..., Strategy = "Hierarchical")}.

      \item \code{"HDBSCAN"}: hierarchical density-based clustering through
      \code{HierarchicalDBSCAN()}.
    }

    Default is \code{"SingleL"}.
  }

  \item{Fast}{
    Logical scalar. Passed to the ordinary lower-level hierarchical clustering
    implementations. If supported, a faster implementation such as
    \pkg{fastcluster} may be used. Specialized methods manage their own
    implementations. Default is \code{TRUE}.
  }
  \item{PlotIt}{
      If TRUE and ClusterNo>0 plots dendrogramm using \code{ClusterDendrogram}
      
      If TRUE and ClusterNo==0 plots black dendrogramm using base R plot
      
      FALSE: No plot
  }
  
  \item{Data}{
    Legacy alias for \code{DataOrDistances}. It is used only when
    \code{DataOrDistances} is missing.
  }

  \item{\dots}{
    Further arguments passed to the selected lower-level clustering function.
  }
}
\value{
A method-specific list.

For ordinary hierarchical methods and \code{"WardPseudo"}, the result is
normally a list with components:

\item{Cls}{If, ClusterNo>0: [1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering. Otherwise for ClusterNo=0: NULL}


\item{Dendrogram}{
  Object of class \code{"dendrogram"}.
}

\item{Object}{
  Underlying hierarchical clustering object, typically of class
  \code{"hclust"}.
}

For \code{"HDBSCAN"}, the result additionally contains
\code{OriginalObject}, which stores the original object returned by
\code{HierarchicalDBSCAN()}.

Other specialized methods return the corresponding method-specific list from
their lower-level implementation.
}
\details{
\strong{Input dispatch}

The wrapper distinguishes raw data from precomputed distances or dissimilarities.

A \code{dist} object is always treated as precomputed distances or dissimilarities. A square
matrix or data frame is treated as a distance matrix only when it is numeric,
finite, symmetric within numerical tolerance, has a zero diagonal, and is
non-negative within tolerance. This reduces the risk of accidentally treating
a square raw-data matrix as a distance matrix.

Precomputed distances or dissimilarities are sent to
\code{HierarchicalClusterDists()}. Raw data are sent to
\code{HierarchicalClusterData()}.

\strong{WardPseudo}

\code{"WardPseudo"} exposes the generalized Ward-like dissimilarity
pseudo-inertia criterion implemented by
\code{HierarchicalClusterDists()}.

It is especially useful when the desired dissimilarity is not Euclidean. For
example, Minkowski \eqn{p = 6} dissimilarities can be calculated explicitly
before clustering:

\preformatted{
D6 <- stats::dist(scale(X), method = "minkowski", p = 6)

res <- HierarchicalClustering(
  D6,
  ClusterNo = 6,
  Type = "WardPseudo"
)
}

The supplied dissimilarities must be unsquared. \code{"WardPseudo"} is distinct
from applying the ordinary legacy Ward method directly to raw non-Euclidean
distances and is also distinct from de Amorim's Ward_p algorithm.

\strong{Specialized methods}

The choices \code{"MinEnergy"}, \code{"Gini"}, \code{"Minimax"},
\code{"Sparse"}, and \code{"HDBSCAN"} are dispatched directly to their
specialized implementations.

For \code{"HDBSCAN"}, if \code{ClusterNo > 1}, the returned hierarchy is cut
with \code{stats::cutree()}. Otherwise, the automatically selected classes
from \code{HierarchicalDBSCAN()} are returned.

\strong{Backward compatibility}

The argument \code{Data} remains available as a legacy alias for
\code{DataOrDistances}. If neither is supplied, the function stops.

The public choice \code{"Ward"} retains its established meaning corresponding
to Ward.D2 behavior. Use \code{"WardPseudo"} explicitly when the generalized
dissimilarity pseudo-inertia criterion is desired.
}
\note{
The mathematical details of the generalized Ward pseudo-inertia criterion,
including its height scale and relationship to Ward.D2, are documented in
\code{\link{HierarchicalClusterDists}}.
}
\examples{
\dontrun{
data("Hepta")

# Raw data with ordinary single linkage
res_single <- HierarchicalClustering(
  DataOrDistances = Hepta$Data,
  ClusterNo = 7,
  Type = "SingleL"
)

# Ward clustering from raw data
res_ward <- HierarchicalClustering(
  Hepta$Data,
  ClusterNo = 7,
  Type = "Ward"
)

# WardPseudo with explicitly computed Minkowski p = 6 dissimilarities
D6 <- stats::dist(
  scale(Hepta$Data),
  method = "minkowski",
  p = 6
)

res_pseudo <- HierarchicalClustering(
  D6,
  ClusterNo = 6,
  Type = "WardPseudo"
)

# HDBSCAN with automatic cluster-number selection
res_hdbscan <- HierarchicalClustering(
  Hepta$Data,
  Type = "HDBSCAN"
)
}
}
\seealso{
\code{\link{HierarchicalClusterDists}},
\code{\link{HierarchicalClusterData}},
\code{\link{HierarchicalDBSCAN}}
}
\author{
Michael Thrun
}
\keyword{Hierarchical}
\keyword{Clustering}
