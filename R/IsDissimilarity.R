IsDissimilarity = function(x) {
  ###############################################################################
  # Check=IsDissimilarity(x)
  #
  # Checks whether an object can reasonably be interpreted as a dissimilarity representation.
  #
  # A valid object is either:
  #   1) an object inheriting from class "dist", or
  #   2) a finite numeric square matrix/data.frame that is approximately
  #      symmetric, has an approximately zero diagonal, and contains no
  #      materially negative values.
  #
  # INPUT
  # x               Object to test. Typically a stats::dist object, matrix,
  #                 data.frame, or arbitrary R object.
  #
  # OUTPUT
  # Logical scalar:
  # TRUE            x is recognized as a distance representation.
  # FALSE           x does not satisfy the required distance-matrix properties.
  #
  # DETAILS
  # Objects inheriting from class "dist" are accepted immediately.
  #
  # Matrix/data.frame input must satisfy all of the following:
  # - numeric
  # - two-dimensional
  # - square
  # - at least 2 x 2
  # - no NA values
  # - all values finite
  #
  # Numerical tolerance is defined as:
  #
  #   tolerance =
  #     100 * .Machine$double.eps * max(1, max(abs(x)))
  #
  # Within this tolerance, the matrix must satisfy:
  # - symmetry:
  #     max(abs(x - t(x))) <= tolerance
  # - zero diagonal:
  #     max(abs(diag(x))) <= tolerance
  # - non-negative entries:
  #     min(x) >= -tolerance
  #
  # Requiring symmetry together with a zero diagonal and non-negative entries is
  # more restrictive than using isSymmetric() alone and reduces the risk of
  # accidentally interpreting a square symmetric raw-data matrix as a distance
  # matrix.
  #
  # NOTE
  # Small negative values caused only by floating-point round-off are tolerated,
  # but the function does not modify or truncate them.
  #
  # author: Michael Thrun
  ###############################################################################
  

  
  
  if (inherits(x, "dist")) {
    return(TRUE)
  }
  
  if (!(is.matrix(x) || is.data.frame(x))) {
    return(FALSE)
  }
  
  x = as.matrix(x)
  
  if (!is.numeric(x) || length(dim(x)) != 2L ||nrow(x) != ncol(x) || nrow(x) < 2L ||anyNA(x) || any(!is.finite(x)) ) {
    return(FALSE)
  }
  
  # More reliable than calling isSymmetric() directly on every possible input.
  # Requiring a zero diagonal and nonnegative entries also reduces accidental
  # classification of a square, symmetric data matrix as a distance matrix.
  tolerance = 100 * .Machine$double.eps * max(1, max(abs(x)))
  
  Check =max(abs(x - t(x))) <= tolerance && #Checks symmetry.
    max(abs(diag(x))) <= tolerance && #Checks that the diagonal is essentially zero.
    min(x) >= -tolerance #Checks that there are no meaningfully negative values
  
  return(Check)
}