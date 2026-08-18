ClusterAlignLabels = function(Cls_reference, Cls_candidate) {
  # ClusterAlignLabels(Cls_reference, Cls_candidate)
  #
  # Aligns cluster labels from a Cls_candidate clustering to a Cls_reference clustering
  # by solving a one-to-one assignment problem that maximizes total overlap.
  #
  # Cluster labels themselves are arbitrary identifiers. Therefore, two
  # clusterings can describe the same partition while using different numeric
  # labels. This function finds the label permutation of Cls_Cls_candidate that
  # yields the largest possible agreement with Cls_Cls_reference.
  #
  # INPUT
  # Cls_reference    Numeric vector [1:n]. Cls_reference cluster labels defining the
  #                  target labeling.
  #
  # Cls_candidate    Numeric vector [1:n]. Cls_candidate cluster labels to be aligned
  #                  to Cls_Cls_reference.
  #
  # OUTPUT
  # List with components:
  #
  # Cls_aligned          Numeric vector [1:n]. Relabeled Cls_candidate clustering.
  #                  Observation order is unchanged.
  #
  # Mapping          data.frame with one row per Cls_candidate cluster and columns:
  #                  - Cls_candidate_label
  #                  - Cls_reference_label
  #                  - matched_observations
  #
  # Matches_before   Integer. Number of observations whose labels were identical
  #                  before alignment.
  #
  # Matches_after    Integer. Number of observations whose labels are identical
  #                  after optimal relabeling.
  #
  # Agreement_before Numeric scalar in [0,1]. Proportion of equal labels before
  #                  alignment.
  #
  # Agreement_after  Numeric scalar in [0,1]. Proportion of equal labels after
  #                  alignment.
  #
  # Overlap_matrix   Integer matrix. Rows correspond to Cls_candidate labels and
  #                  columns to Cls_reference labels. Entry [i,j] counts the number
  #                  of observations assigned to both Cls_candidate class i and
  #                  Cls_reference class j.
  #
  # DETAILS
  # - The function constructs a contingency/overlap matrix between Cls_candidate and
  #   Cls_reference labels.
  # - clue::solve_LSAP(..., maximum = TRUE) solves the linear sum assignment
  #   problem and chooses a unique Cls_reference label for every Cls_candidate label.
  # - The assignment maximizes the total number of matched observations.
  # - Cls_candidate labels are replaced according to this mapping without changing
  #   observation order.

  # author: Michael Thrun
  
  if (!requireNamespace("clue", quietly = TRUE)) {
    stop(
      "Package 'clue' is required. Install it with:\n",
      "install.packages('clue')"
    )
  }
  
  if (!is.numeric(Cls_reference) || !is.numeric(Cls_candidate)) {
    stop("Both inputs must be numeric vectors.")
  }
  
  if (length(Cls_reference) != length(Cls_candidate)) {
    stop("The vectors must have the same length.")
  }
  
  if (length(Cls_reference) == 0L) {
    stop("The vectors must not be empty.")
  }
  
  if (anyNA(Cls_reference) || anyNA(Cls_candidate)) {
    stop("Missing values are not supported.")
  }
  
  Cls_reference_labels = sort(unique(Cls_reference))
  Cls_candidate_labels = sort(unique(Cls_candidate))
  
  if (length(Cls_reference_labels) != length(Cls_candidate_labels)) {
    stop(
      "A one-to-one relabeling requires the same number ",
      "of distinct classes in both vectors."
    )
  }
  
  # Rows: labels currently used in Cls_candidate
  # Columns: labels used in Cls_reference
  overlap_matrix = as.matrix(
    table(
      Cls_candidate = factor(Cls_candidate, levels = Cls_candidate_labels),
      Cls_reference = factor(Cls_reference, levels = Cls_reference_labels)
    )
  )
  
  # For every Cls_candidate class, choose a unique Cls_reference class
  # so that the total overlap is maximal.
  assignment = as.integer(
    clue::solve_LSAP(overlap_matrix, maximum = TRUE)
  )
  
  replacement_labels = Cls_reference_labels[assignment]
  
  # Relabel Cls_candidate without changing observation order
  aligned_Cls_candidate = replacement_labels[
    match(Cls_candidate, Cls_candidate_labels)
  ]
  
  selected_overlaps = overlap_matrix[
    cbind(seq_along(Cls_candidate_labels), assignment)
  ]
  
  return(list(
    Cls_aligned = aligned_Cls_candidate,
    
    Mapping = data.frame(
      Cls_candidate_label = Cls_candidate_labels,
      Cls_reference_label = replacement_labels,
      matched_observations = as.integer(selected_overlaps),
      row.names = NULL
    ),
    
    Matches_before = sum(Cls_reference == Cls_candidate),
    Matches_after = sum(Cls_reference == aligned_Cls_candidate),
    
    Agreement_before = mean(Cls_reference == Cls_candidate),
    Agreement_after = mean(Cls_reference == aligned_Cls_candidate),
    
    Overlap_matrix = overlap_matrix
  ))
}