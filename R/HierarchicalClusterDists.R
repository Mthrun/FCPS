HierarchicalClusterDists = function(pDist, ClusterNo = 0,Type = "ward.D2", ColorTreshold = 0,Fast = FALSE,PlotIt=FALSE,...) {
  # HierarchicalClusterDists(pDist)
  # HierarchicalClusterDists(pDist, 0, "ward.D2", 100)
  # res = HierarchicalClusterDists(pDist, 6, "ward.D2")
  #
  # Minkowski L6 example:
  #
  # Xscaled = scale(X)
  # D6 = stats::dist(
  #   Xscaled,
  #   method = "minkowski",
  #   p = 6
  # )
  #
  # res = HierarchicalClusterDists(
  #   D6,
  #   ClusterNo = 6,
  #   Type = "ward.pseudo"
  # )
  #
  #
  # DESCRIPTION
  #
  # Performs agglomerative hierarchical clustering from a precomputed
  # dissimilarity object.
  #
  # If ClusterNo == 0, the dendrogram is plotted and returned.
  # If ClusterNo > 0, the hierarchy is cut into ClusterNo clusters and
  # the corresponding class assignment is returned.
  #
  #
  # INPUT
  #
  # pDist
  #   Precomputed pairwise dissimilarities supplied as either:
  #
  #   1. an object of class "dist", or
  #   2. a finite, nonnegative, symmetric n x n dissimilarity matrix
  #      with a zero diagonal.
  #
  #   IMPORTANT:
  #   A raw n x d feature matrix is not converted into distances by this
  #   function. Compute the distances beforehand, for example:
  #
  #     D6 = stats::dist(X, method = "minkowski", p = 6)
  #
  #   For Type = "ward.pseudo", pDist must contain the original,
  #   UNSQUARED dissimilarities.
  #
  #
  # OPTIONAL
  #
  # ClusterNo
  #   Number of clusters to return.
  #
  #   ClusterNo = 0:
  #     plot and return the complete hierarchy.
  #
  #   ClusterNo > 0:
  #     cut the hierarchy into ClusterNo groups.
  #
  #
  # Type
  #   Agglomeration method. Supported values are:
  #
  #     "ward.D"
  #     "ward.D2"
  #     "ward.pseudo"
  #     "single"
  #     "complete"
  #     "average"
  #     "mcquitty"
  #     "median"
  #     "centroid"
  #
  #   "ward.D"
  #     Applies R's legacy ward.D Lance-Williams update directly to
  #     pDist. This should not be used on raw Minkowski p = 6
  #     dissimilarities as a justification of the generalized Ward
  #     pseudo-inertia criterion.
  #
  #   "ward.D2"
  #     R's Ward.D2 implementation. The supplied dissimilarities are
  #     squared internally before the Ward update.
  #
  #     For arbitrary dissimilarities this can be interpreted as a
  #     Ward-like hierarchy based on squared dissimilarities. Its
  #     reported heights are on a square-root, distance-like scale.
  #
  #   "ward.pseudo"
  #     Implements the dissimilarity-based generalized Ward criterion
  #     described by Chavent et al. (2018).
  #
  #     Let w_i be the weight of observation i and define the weight of
  #     a cluster C as
  #
  #       mu_C = sum_{i in C} w_i.
  #
  #     The dissimilarity-based pseudo-inertia of cluster C is
  #
  #       I_D(C) =
  #         sum_{i,j in C} w_i * w_j * d_ij^2 / (2 * mu_C).
  #
  #     At every agglomeration step, the clusters A and B minimizing
  #
  #       Delta_D(A, B) =
  #         I_D(A union B) - I_D(A) - I_D(B)
  #
  #     are merged.
  #
  #     This function uses equal normalized observation weights
  #
  #       w_i = 1 / n.
  #
  #     Consequently, the initial aggregation costs are
  #
  #       Delta_ij = d_ij^2 / (2 * n).
  #
  #     These aggregation costs are passed internally to the ward.D
  #     Lance-Williams update together with members = rep(1/n, n).
  #
  #     The internal use of ward.D is therefore applied to transformed
  #     aggregation costs, NOT to the original dissimilarities.
  #
  #     The criterion is defined for potentially non-Euclidean
  #     dissimilarities, including Minkowski p = 6 dissimilarities.
  #
  #     For equal observation weights, ward.pseudo and ward.D2 applied
  #     to the same original dissimilarities produce the same sequence
  #     of cluster mergers, apart from possible ordering differences
  #     caused by exact numerical ties.
  #
  #     Their height scales differ:
  #
  #       height_ward.pseudo =
  #         height_ward.D2^2 / (2 * n).
  #
  #     The heights returned for ward.pseudo are increases in normalized
  #     pseudo within-cluster inertia.
  #
  #     This option does NOT implement the Ward_p algorithm of
  #     de Amorim (2015). Ward_p is a different algorithm involving
  #     cluster-dependent feature weights and Lp cluster centers.
  #
  #
  # ColorTreshold
  #   Legacy spelling retained for backward compatibility.
  #
  #   If nonzero, rectangles are drawn at the corresponding dendrogram
  #   height. The numerical scale depends on Type.
  #
  #   A threshold used for ward.D2 cannot be transferred unchanged to
  #   ward.pseudo. For the same cut-height scale, use
  #
  #     threshold.pseudo = threshold.ward.D2^2 / (2 * n).
  #
  #   If the hierarchy has non-monotone fusion heights, a horizontal
  #   reference line is drawn instead of calling rect.hclust().
  #
  #
  # Fast
  #   If TRUE and package "fastcluster" is installed,
  #   fastcluster::hclust() is used.
  #
  #   Otherwise stats::hclust() is used.
  #
  #
  # ...
  #   Additional arguments passed to plot.dendrogram().
  #
  #
  # OUTPUT
  #
  # A list with:
  #
  # Cls           If, ClusterNo>0: [1:n]  numerical vector with n numbers defining the classification
  #               as the main output of the clustering algorithm. 
  #               It has k unique numbers representing the arbitrary labels of the clustering. 
  #               Otherwise for ClusterNo=0: NULL
  #
  #
  # Dendrogram
  #   Object of class "dendrogram".
  #
  # Object
  #   Object of class "hclust".
  #
  #   For Type = "ward.pseudo", Object$height contains the successive
  #   increases in normalized pseudo within-cluster inertia.
  #
  #
  # REFERENCES
  #
  # Chavent, M., Kuentz-Simonet, V., Labenne, A., & Saracco, J. (2018).
  # ClustGeo: an R package for hierarchical clustering with spatial
  # constraints. Computational Statistics, 33, 1799-1822.
  # doi:10.1007/s00180-018-0791-1
  #
  # Murtagh, F., & Legendre, P. (2014).
  # Ward's hierarchical agglomerative clustering method:
  # Which algorithms implement Ward's criterion?
  # Journal of Classification, 31, 274-295.
  # doi:10.1007/s00357-014-9161-z
  #
  # RELATED METHOD, NOT IMPLEMENTED HERE
  #
  # de Amorim, R. C. (2015).
  # Feature relevance in Ward's hierarchical clustering using the Lp norm.
  # Journal of Classification, 32, 46-62.
  # doi:10.1007/s00357-015-9167-1
  #
  #
  # Author: MT
  
  supported_types = c(
    "ward.D",
    "ward.D2",
    "ward.pseudo",
    "single",
    "complete",
    "average",
    "mcquitty",
    "median",
    "centroid",
    "MinEnergy",
    "Gini",
    "Genie",
    "Minimax",
    "Sparse"
  )
  
  # ---------------------------------------------------------------
  # Validate Type
  # ---------------------------------------------------------------
  
  if ( !is.character(Type) || length(Type) != 1L || is.na(Type) ||!(Type %in% supported_types) ) {
    stop(
      "Unknown 'Type'. Supported values are: ",
      paste(shQuote(supported_types), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------
  # Validate Fast
  # ---------------------------------------------------------------
  
  if ( !is.logical(Fast) ||  length(Fast) != 1L || is.na(Fast) ) {
    stop(  "'Fast' must be either TRUE or FALSE.", call. = TRUE )
  }
  
  # ---------------------------------------------------------------
  # Convert and validate dissimilarities
  # ---------------------------------------------------------------
  
  if (!inherits(pDist, "dist")) {
    if (!(is.matrix(pDist) || is.data.frame(pDist))) {
      stop( "'pDist' must be an object of class 'dist' or a square ", "dissimilarity matrix.", call. = TRUE  )
    }
    
    dmat = as.matrix(pDist)
    
    if ( !is.numeric(dmat) || length(dim(dmat)) != 2L ||  nrow(dmat) != ncol(dmat) ||  nrow(dmat) < 2L  ) {
      stop("A matrix supplied as 'pDist' must be numeric, square, ",
        "and contain at least two observations. For raw feature ",
        "data, calculate distances first with stats::dist().", call. = TRUE )
    }
    
    if (anyNA(dmat) || any(!is.finite(dmat))) {
      stop( "'pDist' contains missing or non-finite values.", call. = TRUE )
    }
    
    matrix_tolerance = 100 * .Machine$double.eps *
      max(1, max(abs(dmat)))
    
    if (max(abs(dmat - t(dmat))) > matrix_tolerance) {
      stop( "A dissimilarity matrix supplied as 'pDist' must be symmetric.",call. = TRUE)
    }
    
    if (max(abs(diag(dmat))) > matrix_tolerance) {
      stop("A dissimilarity matrix supplied as 'pDist' must have ","a zero diagonal.", call. = TRUE )
    }
    
    if (min(dmat) < -matrix_tolerance) {
      stop( "'pDist' must contain nonnegative dissimilarities.", call. = TRUE)
    }
    
    # Remove negligible numerical asymmetry and negative round-off.
    dmat = (dmat + t(dmat)) / 2
    dmat[dmat < 0] = 0
    diag(dmat) = 0
    
    
    # Specialized hierarchical methods
    if(Type=="MinEnergy"){
      return(MinimalEnergyClustering(DataOrDistances=dmat,ClusterNo=ClusterNo,
                                     ColorTreshold=ColorTreshold,
                                     PlotIt=PlotIt,...))
    }else if(Type %in% c("Gini","Genie")){
      return(GenieClustering(DataOrDistances=dmat,ClusterNo=ClusterNo,
                             ColorTreshold=ColorTreshold,
                             PlotIt=PlotIt,...))
    }else if(Type=="Minimax"){
      return(MinimaxLinkageClustering(DataOrDistances=dmat,ClusterNo=ClusterNo,
                                      ColorTreshold=ColorTreshold,
                                      PlotIt=PlotIt,...))
    }else if(Type=="Sparse"){
      return(SparseClustering(DataOrDistances=dmat,ClusterNo=ClusterNo,
                              Type="Hierarchical",PlotIt=PlotIt,
                              ColorTreshold=ColorTreshold,...))
    }
    
    pDist = stats::as.dist(dmat)
  }
  
  n = attr(pDist, "Size", exact = TRUE)
  
  if (is.null(n) ||length(n) != 1L ||  is.na(n) ||!is.finite(n) || n < 2) {
    stop("'pDist' must describe at least two observations.",  call. = TRUE  )
  }
  
  n = as.integer(n)
  
  expected_length = n * (n - 1) / 2
  
  if (length(pDist) != expected_length) {
    stop( "'pDist' has an invalid length for a distance object of size ", n,".", call. = TRUE)
  }
  
  distance_values = as.numeric(pDist)
  
  if (anyNA(distance_values) || any(!is.finite(distance_values))) {
    stop(  "'pDist' contains missing or non-finite values.", call. = TRUE )
  }
  
  distance_tolerance = 100 * .Machine$double.eps *
    max(1, max(abs(distance_values)))
  
  if (min(distance_values) < -distance_tolerance) {
    stop( "'pDist' must contain nonnegative dissimilarities.", call. = TRUE  )
  }
  
  if (any(distance_values < 0)) {
     pDist[] = pmax(distance_values, 0)
    distance_values = as.numeric(pDist)
  }
  
  # ---------------------------------------------------------------
  # Validate ClusterNo
  # ---------------------------------------------------------------
  
  if ( !is.numeric(ClusterNo) || length(ClusterNo) != 1L || is.na(ClusterNo) || !is.finite(ClusterNo) ||ClusterNo < 0 || ClusterNo > n || ClusterNo != floor(ClusterNo) ) {
    stop("'ClusterNo' must be an integer between 0 and ",  n,  ".",   call. = TRUE  )
  }
  
  ClusterNo = as.integer(ClusterNo)
  
  # ---------------------------------------------------------------
  # Validate ColorTreshold
  # ---------------------------------------------------------------
  
  if (!is.numeric(ColorTreshold) || length(ColorTreshold) != 1L ||  is.na(ColorTreshold) || !is.finite(ColorTreshold)
  ) {
    stop(  "'ColorTreshold' must be one finite numeric value.",  call. = TRUE )
  }
  
  ColorTreshold = as.numeric(ColorTreshold)
  
  # ---------------------------------------------------------------
  # Select hclust implementation
  # ---------------------------------------------------------------
  
  use_fast = isTRUE(Fast) && requireNamespace("fastcluster", quietly = TRUE)
  
  if (isTRUE(Fast) && !use_fast) {
    warning( "Package 'fastcluster' is not installed; ", "using stats::hclust() instead.",  call. = TRUE )
  }
  
  if (use_fast) {
    hclust_function =fastcluster::hclust
  } else {
    hclust_function = stats::hclust
  }
  
  input_distance_method = attr( pDist,"method", exact = TRUE )
  
  if (is.null(input_distance_method) ||length(input_distance_method) != 1L ||is.na(input_distance_method) ||!nzchar(as.character(input_distance_method)) ) {
    input_distance_method = "user-supplied"
  } else {
    input_distance_method = as.character(input_distance_method)
  }
  
  # ---------------------------------------------------------------
  # Clustering
  # ---------------------------------------------------------------
  
  if (identical(Type, "ward.pseudo")) {
     observation_weights = rep(1 / n, n)
    
    # Chavent et al. singleton aggregation costs:
    #
    #   Delta_ij =
    #     (w_i * w_j) / (w_i + w_j) * d_ij^2
    #
    # For equal weights w_i = 1/n:
    #
    #   Delta_ij = d_ij^2 / (2*n).
    #
    # Preserve the attributes and class of the dist object.
    aggregation_dist = pDist
    aggregation_dist[] = distance_values^2 / (2 * n)
    
    # Important:
    # ward.D is applied to the aggregation costs, not to raw pDist.
    hc = hclust_function(
      aggregation_dist,
      method = "ward.D",
      members = observation_weights
    )
    
    # Describe the exposed criterion rather than the internal
    # Lance-Williams implementation detail.
    hc$method = "ward.pseudo"
    hc$dist.method = input_distance_method
    hc$call = match.call()
    
    plot_title = sprintf( "Ward-like pseudo-inertia clustering, k = %d",  ClusterNo  )
    plot_y_label = paste("Increase of ultrametric Portion in pseudo within-cluster inertia" )
  } else {
    hc = hclust_function( pDist, method = Type)
    
    hc$dist.method = input_distance_method
    hc$call = match.call()
    
    plot_title = sprintf(  "%s clustering, k = %d", Type,ClusterNo)
    plot_y_label = "Ultrametric Portion of Distance"
  }
  plot_x_label =  sprintf("No. of Data Points N = %d",n)
  
  dendrogram = stats::as.dendrogram(hc)
  
  # ---------------------------------------------------------------
  # Return classification
  # ---------------------------------------------------------------
  
  if (ClusterNo > 0L) {
    Cls = stats::cutree(  hc, k = ClusterNo)
    if(isTRUE(PlotIt)){
      V=ClusterDendrogram(TreeOrDendrogram=dendrogram,ClusterNo=ClusterNo,main=plot_title,ylab=plot_y_label,xlab=plot_x_label)
    }
    return(
      list( Cls = Cls, Dendrogram = dendrogram,  Object = hc  )
    )
  }
  
  # ---------------------------------------------------------------
  # Plot dendrogram
  # ---------------------------------------------------------------
  if(isTRUE(PlotIt)){
    graphics::plot(
      dendrogram,
      main = plot_title,
      xlab = "Number of data points N",
      ylab = plot_y_label,
      sub = " ",
      leaflab = "none",
    )
    
    graphics::axis(
      side = 1,
      col = "black",
      las = 1
    )
  }
  # ---------------------------------------------------------------
  # Draw threshold
  # ---------------------------------------------------------------
  
  if (ColorTreshold != 0) {
    height_tolerance = sqrt(.Machine$double.eps) * max(1, max(abs(hc$height)))
    
    monotone_heights = all(diff(hc$height) >= -height_tolerance )
    
    if (monotone_heights) {
      if(isTRUE(PlotIt)){
        stats::rect.hclust( hc, h = ColorTreshold, border = "red" )
      }
      Cls <- stats::cutree(hc, h = ColorTreshold)
    } else {
      if(isTRUE(PlotIt)){
        graphics::abline( h = ColorTreshold, col = "red" )
      }
      Cls=NULL
      warning( "The hierarchy has non-monotone fusion heights. ",
        "Only a horizontal threshold line was drawn. ",
        "Use ClusterNo rather than a height cut to obtain ",
        "a partition.", call. = TRUE )
    }
  }else{
    Cls=NULL
  }
  
  return(list(Cls = Cls,Dendrogram = dendrogram,Object = hc))
}