HierarchicalClustering = function( DataOrDistances,ClusterNo = 0,Type = "SingleL",Fast = TRUE,PlotIt=FALSE, Data,...) {
  # HierarchicalClustering(DataOrDistances, ClusterNo, Type = "SingleL")
  #
  # DESCRIPTION
  #   Unified wrapper for hierarchical clustering of either a data matrix or
  #   precomputed dissimilarities. Public method names are translated to the
  #   canonical names used by the lower-level clustering functions.
  #
  # INPUT
  #   DataOrDistances
  #     Either:
  #       * an n x d data matrix/data frame, or
  #       * precomputed dissimilarities as a "dist" object or a finite,
  #         nonnegative, symmetric n x n matrix with zero diagonal.
  #
  #     For Type = "WardPseudo" (canonical name: "ward.pseudo"), this input
  #     MUST contain the original, unsquared dissimilarities. For example:
  #
  #       D6 = stats::dist(scale(X), method = "minkowski", p = 6)
  #       res = HierarchicalClustering(
  #         D6,
  #         ClusterNo = 6,
  #         Type = "WardPseudo"
  #       )
  #
  #     Raw feature data are deliberately not accepted for WardPseudo because
  #     this wrapper has no distance-metric or Minkowski-p argument. Computing
  #     the dissimilarities explicitly keeps the selected metric unambiguous.
  #
  #   ClusterNo
  #     Number of clusters. ClusterNo = 0 delegates dendrogram handling to the
  #     selected lower-level function.
  #
  # OPTIONAL
  #   Type
  #     Public aliases for ordinary hierarchical methods:
  #
  #       "Ward"        -> "ward.D2"
  #       "WardPseudo"  -> "ward.pseudo"
  #       "SingleL"     -> "single"
  #       "CompleteL"   -> "complete"
  #       "AverageL"    -> "average"  (UPGMA)
  #       "WPGMA"       -> "mcquitty"
  #       "MedianL"     -> "median"   (WPGMC)
  #       "CentroidL"   -> "centroid" (UPGMC)
  #
  #     The canonical lower-level names are also accepted directly.
  #
  #     "WardPseudo" applies the generalized Ward-like dissimilarity
  #     pseudo-inertia criterion implemented by HierarchicalClusterDists.
  #     It is intended for precomputed, potentially non-Euclidean
  #     dissimilarities such as Minkowski p = 6 distances. It is distinct from
  #     both stats::hclust(..., method = "ward.D") on the raw distances and
  #     de Amorim's Ward_p algorithm.
  #
  #     Additional wrapper methods:
  #       "Minimax", "MinEnergy", "Gini"/"Genie", "Sparse", "HDBSCAN".
  #
  #   Fast
  #     Passed to HierarchicalClusterDists or HierarchicalClusterData. The
  #     specialized methods listed above manage their own implementations.
  #
  #   Data
  #     Legacy alias for DataOrDistances. Used only when DataOrDistances is
  #     missing.
  #
  #   ...
  #     Additional arguments passed to the selected lower-level function.
  #
  # OUTPUT
  #   A method-specific list. For the ordinary hierarchical methods and
  #   WardPseudo this is the result returned by HierarchicalClusterDists or
  #   HierarchicalClusterData, normally containing Cls, Dendrogram and Object.
  #
  # NOTES
  #   "Ward" retains the existing meaning "ward.D2" for backward
  #   compatibility. Use "WardPseudo" explicitly for the generalized
  #   dissimilarity pseudo-inertia criterion.
  #
  #   The full mathematical definition and references belong in
  #   HierarchicalClusterDists, where "ward.pseudo" is implemented.
  #
  # Author: MT, 04/2018; WardPseudo integration added 2026

  if (missing(DataOrDistances)) {
    if (missing(Data)) {
      stop("Either 'DataOrDistances' or the legacy argument 'Data' must be supplied.",call. = TRUE)
    }
    DataOrDistances = Data
  }

  if (
    !is.character(Type) ||
    length(Type) != 1L ||
    is.na(Type) ||
    !nzchar(Type)
  ) {
    stop("'Type' must be one non-empty character value.", call. = TRUE)
  }

  # Public API -> canonical lower-level names.
  type_aliases = c(
    Ward = "ward.D2",
    WardPseudo = "ward.pseudo",
    SingleL = "single",
    CompleteL = "complete",
    AverageL = "average",
    WPGMA = "mcquitty",
    MedianL = "median",
    CentroidL = "centroid"
  )

  requested_type = Type

  if (Type %in% names(type_aliases)) {
    Type = unname(type_aliases[[Type]])
  }

  # More reliable than calling isSymmetric() directly on every possible input.
  # Requiring a zero diagonal and nonnegative entries also reduces accidental
  # classification of a square, symmetric data matrix as a distance matrix.
  is_distance_input = function(x) {
    if (inherits(x, "dist")) {
      return(TRUE)
    }

    if (!(is.matrix(x) || is.data.frame(x))) {
      return(FALSE)
    }

    x = as.matrix(x)

    if (
      !is.numeric(x) ||
      length(dim(x)) != 2L ||
      nrow(x) != ncol(x) ||
      nrow(x) < 2L ||
      anyNA(x) ||
      any(!is.finite(x))
    ) {
      return(FALSE)
    }

    tolerance = 100 * .Machine$double.eps * max(1, max(abs(x)))

    max(abs(x - t(x))) <= tolerance &&
      max(abs(diag(x))) <= tolerance &&
      min(x) >= -tolerance
  }

  distance_input = is_distance_input(DataOrDistances)

  # WardPseudo is intentionally distance-only at this API level. Otherwise a
  # raw matrix would be sent to HierarchicalClusterData, where the distance
  # metric (and in particular Minkowski p = 6) is not specified by this wrapper.
  if (identical(Type, "ward.pseudo") && !distance_input) {
    stop(
      paste0(
        "Type = ", shQuote(requested_type),
        " requires precomputed, unsquared dissimilarities ",
        "as a 'dist' object or a symmetric distance matrix. For Minkowski ",
        "p = 6, use dist(scale(X), method = 'minkowski', p = 6) first."
      ),
      call. = TRUE
    )
  }

  # Backwards compatibility to Matlab / specialized implementations.
  if (identical(Type, "MinEnergy")) {
    return(
      MinimalEnergyClustering(
        DataOrDistances = DataOrDistances,
        ClusterNo = ClusterNo,
        PlotIt = PlotIt,
        ...
      )
    )
  } else if (Type %in% c("Gini", "Genie")) {
    return(
      GenieClustering(
        DataOrDistances = DataOrDistances,
        ClusterNo = ClusterNo,
        PlotIt = PlotIt,
        ...
      )
    )
  } else if (identical(Type, "Minimax")) {
    return(
      MinimaxLinkageClustering(
        DataOrDistances = DataOrDistances,
        ClusterNo = ClusterNo,
        PlotIt = PlotIt,
        ...
      )
    )
  } else if (identical(Type, "Sparse")) {
    return(
      SparseClustering(
        DataOrDistances = DataOrDistances,
        ClusterNo = ClusterNo,
        Type = "Hierarchical",
        PlotIt = PlotIt,
        ...
      )
    )
  } else if (identical(Type, "HDBSCAN")) {
    V = HierarchicalDBSCAN(
      DataOrDistances = DataOrDistances,
      ...
    )

    if (ClusterNo > 1) {
      Cls = stats::cutree(V$Tree, ClusterNo)
    } else {
      # Automatic number-of-clusters selection by HierarchicalDBSCAN.
      Cls = V$Cls
    }

    return(
      list(
        Cls = Cls,
        Dendrogram = V$Dendrogram,
        Object = V$Tree,
        OriginalObject = V$Object
      )
    )
  }

  if (distance_input) {
    Input = if (inherits(DataOrDistances, "dist")) {
      DataOrDistances
    } else {
      stats::as.dist(as.matrix(DataOrDistances))
    }

    return(
      HierarchicalClusterDists(
        pDist = Input,
        ClusterNo = ClusterNo,
        Type = Type,
        Fast = Fast,
        PlotIt=PlotIt,
        ...
      )
    )
  }

  # Raw data. WardPseudo has already been rejected above because its chosen
  # dissimilarity must be explicit.
  HierarchicalClusterData(
    Data = DataOrDistances,
    ClusterNo = ClusterNo,
    Type = Type,
    Fast = Fast,
    PlotIt = PlotIt,
    ...
  )
}
