BalancedKmeansClustering <- function(Data, ClusterNo = 2,
                                    MaxDiff = 1, IterMax = 1000,
                                    Switch = 10, Nstart = 1, Centers = NULL,
                                    Seed = NULL,
                                    PartlyRemainingFraction = 0.15,
                                    IncreasingPenaltyFactor = 1.01,
                                    UseFunctionIter = TRUE,
                                    StopWhenBalanced = FALSE,
                                    PlotIt = FALSE, Verbose = FALSE,
                                    KeepHistory = TRUE) {
  ###############################################################################
  # V=BalancedKmeansClustering(Data, ClusterNo)
  # Balanced k-means revisited (BKM+) performs balanced k-means
  # clustering using an independent base-R implementation of BKM+.
  #
  # The algorithm combines squared-Euclidean k-means optimization with an
  # adaptive cluster-size penalty. It searches for a partition whose difference
  # between the largest and smallest cluster sizes does not exceed MaxDiff, while
  # retaining a low within-cluster sum of squares.
  #
  # INPUT
  # Data
  #   Feature values to cluster. Pairwise distances are not supported because
  #   BKM+ updates arithmetic centroids in the original feature space.
  #
  #   Accepted input:
  #   - real numeric matrix with observations in rows and features in columns;
  #   - data.frame whose columns are all real numeric;
  #   - real numeric vector, converted to a one-column matrix.
  #
  #   The input must be non-empty and contain only finite values. Objects of class
  #   "dist" and matrices recognized as dissimilarity matrices are rejected.
  #
  # OPTIONAL
  # ClusterNo
  #   Positive integer number of clusters. Must lie between 1 and nrow(Data).
  #   Default: 2.
  #
  # MaxDiff
  #   Non-negative integer specifying the largest allowed difference between the
  #   largest and smallest cluster sizes:
  #
  #     max(table(Cls)) - min(table(Cls)) <= MaxDiff
  #
  #   Default: 1.
  #
  #   MaxDiff = 0 requests equal cluster sizes and is feasible only when
  #   ClusterNo divides the number of observations.
  #
  # IterMax
  #   Positive integer maximum number of adaptive-penalty iterations per start.
  #   Default: 1000.
  #
  # Switch
  #   Non-negative integer maximum number of post-processing switch passes.
  #
  #   During one switch pass, pairs of observations from two clusters may exchange
  #   their cluster assignments when the exchange decreases the total
  #   within-cluster sum of squares. These exchanges preserve cluster sizes.
  #
  #   Switch = 0 disables switch refinement.
  #   Default: 10.
  #
  # Nstart
  #   Positive integer number of initializations.
  #
  #   If Centers is NULL, every start samples ClusterNo observations without
  #   replacement as initial centers.
  #
  #   Default: 1.
  #
  # Centers
  #   Optional initial cluster-center matrix with:
  #   - ClusterNo rows;
  #   - ncol(Data) columns;
  #   - finite real numeric values.
  #
  #   If Centers is supplied, Nstart must be 1.
  #   Default: NULL.
  #
  # Seed
  #   NULL or a non-negative integer used for random initialization.
  #
  #   An explicit seed is local to this function call: the previous global
  #   .Random.seed is restored when the function exits, including after errors.
  #
  #   With Seed = NULL, random initialization advances the ordinary R random
  #   number stream.
  #
  # PartlyRemainingFraction
  #   Finite numeric scalar strictly between 0 and 1.
  #
  #   Fractional term used in the adaptive size-penalty gap when evaluating the
  #   reassignment of an observation after temporarily removing it from its
  #   current cluster.
  #
  #   Default: 0.15.
  #
  # IncreasingPenaltyFactor
  #   Finite numeric scalar greater than or equal to 1.
  #
  #   Constant multiplier used to increase the next balancing penalty when
  #   UseFunctionIter = FALSE.
  #
  #   Default: 1.01.
  #
  # UseFunctionIter
  #   Logical scalar.
  #
  #   TRUE:
  #     use an iteration-dependent penalty multiplier:
  #
  #       1.1009 - 0.0009 * step   for step <= 100
  #       1.01                     for step > 100
  #
  #   FALSE:
  #     use IncreasingPenaltyFactor.
  #
  #   Default: TRUE.
  #
  # StopWhenBalanced
  #   Logical scalar controlling termination after a feasible size-balanced
  #   assignment has been reached.
  #
  #   If TRUE, a feasible iteration that no longer improves the best feasible
  #   within-cluster sum of squares may terminate the balancing phase.
  #
  #   Independently of this flag, a non-improving solution with cluster-size
  #   difference <= 1 may terminate as hard balanced.
  #
  #   Default: FALSE.
  #
  # PlotIt
  #   Logical scalar. If TRUE, plots the selected clustering with
  #   ClusterPlotMDS().
  #
  #   At least three observations are required for the plot; otherwise a warning
  #   is issued and plotting is skipped.
  #
  #   Default: FALSE.
  #
  # Verbose
  #   Logical scalar. If TRUE, reports the SSE, cluster-size difference, and
  #   termination status of every start.
  #
  #   Default: FALSE.
  #
  # KeepHistory
  #   Logical scalar. If TRUE, retain balancing-iteration and switch-refinement
  #   histories in the returned Object.
  #
  #   Default: TRUE.
  #
  # OUTPUT
  # List with:
  #
  # Cls
  #   Numerical vector [1:n] containing integer-valued cluster labels from
  #   1 to ClusterNo. Element i is the cluster of row i of Data.
  #
  #   The observation order and the algorithm's native cluster numbering are
  #   preserved. Names are copied from rownames(Data) when available;
  #   otherwise "1", ..., "n" are used.
  #
  # Object
  #   Detailed BKM+ result containing:
  #
  #   Cls
  #     Same numerical cluster vector as the main output Cls.
  #
  #   centers
  #     Final ClusterNo x p centroid matrix after switch refinement. Row j
  #     corresponds to cluster j in Cls.
  #
  #   size
  #     Number of observations in each cluster.
  #
  #   withinss
  #     Within-cluster sum of squared errors for each cluster.
  #
  #   tot.withinss
  #     Total within-cluster sum of squared errors.
  #
  #   MSE
  #     tot.withinss / n.
  #
  #   iter
  #     Number of iterations executed in the balancing phase of the selected run.
  #
  #   best.iter
  #     Iteration at which the retained feasible assignment was found, or the
  #     final retained iteration when no feasible assignment was stored.
  #
  #   penalty
  #     Penalty associated with the retained assignment.
  #
  #   last.penalty
  #     Last penalty reached by the balancing iterations.
  #
  #   maxdiff
  #     Final difference between largest and smallest cluster sizes.
  #
  #   balanced
  #     TRUE if maxdiff <= MaxDiff.
  #
  #   hard.balanced
  #     TRUE if maxdiff <= 1.
  #
  #   converged
  #     Logical convergence indicator for the balancing phase.
  #
  #   termination
  #     Character termination reason, e.g. "balanced_no_improvement",
  #     "single_cluster", "no_further_penalty", "penalty_stalled",
  #     "penalty_numerical_limit", or "iteration_limit".
  #
  #   initial.centers
  #     Initial centers of the selected start in cluster-number order 1:ClusterNo.
  #
  #   initial.empty.clusters
  #     Number of empty clusters repaired immediately after initial assignment.
  #
  #   sse.before.switch
  #     Total SSE before the switch-refinement phase.
  #
  #   switch.iter
  #     Number of switch passes performed.
  #
  #   switches
  #     Total number of exchanged observation pairs.
  #
  #   switch.converged
  #     Logical convergence indicator for the switch-refinement phase; NA when
  #     Switch = 0.
  #
  #   history
  #     data.frame of balancing iterations when KeepHistory = TRUE; otherwise
  #     NULL. Columns include iter, penalty, working.sse, maxdiff, balanced, moved.
  #
  #   switch.history
  #     data.frame of switch passes when retained. Columns include iter, switches,
  #     and sse.
  #
  #   nstart
  #     Number of requested starts.
  #
  #   best.start
  #     Index of the selected start.
  #
  #   starts
  #     data.frame summarizing every start: start, tot.withinss, maxdiff,
  #     balanced, converged, iter, termination.
  #
  #   maxdiff.requested
  #     Requested MaxDiff.
  #
  #   parameters
  #     List of principal algorithm parameters.
  #
  #   call
  #     Matched function call.
  #
  # Centroids
  #   Final centroid matrix, identical to Object$centers.
  #
  # DETAILS
  # INITIALIZATION
  # If Centers is NULL, ClusterNo observations are sampled as initial centers.
  # Initial assignment uses squared Euclidean distance and chooses the first
  # cluster in an exact distance tie.
  #
  # Duplicate observations or supplied centers can create empty initial clusters.
  # Every empty cluster is repaired by moving a point with large current squared
  # error from a non-singleton donor cluster. This repair is limited to
  # initialization.
  #
  # ADAPTIVE-PENALTY BALANCING
  # The algorithm performs sequential observation reassignments. A point is never
  # removed from a singleton cluster. Candidate target clusters are evaluated
  # using squared centroid distance together with an adaptive size penalty.
  #
  # The best feasible assignment is retained according to total within-cluster
  # SSE. A saved feasible assignment is not discarded when the iteration limit is
  # reached.
  #
  # SWITCH REFINEMENT
  # After balancing, pairwise switches between clusters are considered. Exchanges
  # are accepted when they reduce total SSE. Because observations are exchanged
  # in pairs, cluster cardinalities remain unchanged.
  #
  # MULTIPLE STARTS
  # Feasible starts always outrank infeasible starts.
  #
  # Among feasible starts:
  #   select the smallest total within-cluster SSE.
  #
  # Among infeasible starts:
  #   select the smallest cluster-size difference, then the smallest SSE.
  #
  # OUTPUT ORDER
  # Cls is returned in the same observation order as Data: Cls[i]
  # is the label of Data[i, ]. No post-fitting ordering or renumbering
  # of cluster labels is applied. Cluster-indexed outputs retain the same native
  # cluster numbering.
  #
  #
  # NUMERICAL SAFEGUARDS
  # Non-finite intermediate calculations cause an error recommending centering
  # and rescaling the feature data and checking supplied centers.
  #
  # REFERENCE
  # Independent base-R implementation of the method of de Maeyer, Sieranoja
  # and Franti (2023), doi:10.3934/aci.2023008. Algorithmic reference:
  # https://github.com/uef-machine-learning/Balanced_k-Means_Revisited
  # See inst/BalancedKmeansClustering-implementation.md for correspondence,
  # numerical safeguards and intentional differences from the C++ program.
  #
  # de Maeyer, Sieranoja and Franti (2023).
  # Balanced k-means revisited.
  # DOI: 10.3934/aci.2023008.
  #
  # author: Michael Thrun
  Call <- match.call()
  x <- .bkm_data(Data, "Data", RejectDistances = TRUE)
  n <- nrow(x)
  k <- .bkm_integer(ClusterNo, "ClusterNo", 1, n)
  MaxDiff <- .bkm_integer(MaxDiff, "MaxDiff", 0)
  IterMax <- .bkm_integer(IterMax, "IterMax", 1)
  Switch <- .bkm_integer(Switch, "Switch", 0)
  Nstart <- .bkm_integer(Nstart, "Nstart", 1)
  if (MaxDiff == 0L && n %% k != 0L) {
    stop("MaxDiff = 0 is infeasible unless ClusterNo divides the number of rows.",
         call. = FALSE)
  }
  if (!is.numeric(PartlyRemainingFraction) || is.complex(PartlyRemainingFraction) ||
      length(PartlyRemainingFraction) != 1L ||
      !is.finite(PartlyRemainingFraction) ||
      PartlyRemainingFraction <= 0 || PartlyRemainingFraction >= 1) {
    stop("PartlyRemainingFraction must be a finite number strictly between 0 and 1.",
         call. = FALSE)
  }
  if (!is.numeric(IncreasingPenaltyFactor) || is.complex(IncreasingPenaltyFactor) ||
      length(IncreasingPenaltyFactor) != 1L ||
      !is.finite(IncreasingPenaltyFactor) || IncreasingPenaltyFactor < 1) {
    stop("IncreasingPenaltyFactor must be a finite number at least 1.",
         call. = FALSE)
  }
  PartlyRemainingFraction <- as.double(PartlyRemainingFraction)
  IncreasingPenaltyFactor <- as.double(IncreasingPenaltyFactor)
  flags <- list(UseFunctionIter = UseFunctionIter,
                StopWhenBalanced = StopWhenBalanced, PlotIt = PlotIt,
                Verbose = Verbose, KeepHistory = KeepHistory)
  for (nm in names(flags)) {
    z <- flags[[nm]]
    if (!is.logical(z) || length(z) != 1L || is.na(z)) {
      stop(sprintf("%s must be exactly TRUE or FALSE.", nm), call. = FALSE)
    }
  }
  if (!is.null(Centers)) {
    Centers <- .bkm_data(Centers, "Centers", RejectDistances = FALSE)
    if (nrow(Centers) != k || ncol(Centers) != ncol(x)) {
      stop("Centers must have ClusterNo rows and the same number of columns as the data.",
           call. = FALSE)
    }
    if (Nstart != 1L) {
      stop("Use Nstart = 1 when supplying Centers.", call. = FALSE)
    }
  }
  # An explicit seed is local to this call, including calls that raise errors.
  # With Seed = NULL, sampling advances the ordinary R random-number stream.
  if (!is.null(Seed)) {
    Seed <- .bkm_integer(Seed, "Seed", 0)
    had.seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had.seed) old.seed <- get(".Random.seed", envir = .GlobalEnv)
    on.exit({
      if (had.seed) {
        assign(".Random.seed", old.seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(Seed)
  }

  best <- NULL
  starts <- vector("list", Nstart)
  for (s in seq_len(Nstart)) {
    initial <- if (is.null(Centers)) {
      x[sample.int(n, k, replace = FALSE), , drop = FALSE]
    } else Centers
    fit <- .bkm_run(x, initial, MaxDiff, IterMax, Switch,
                    PartlyRemainingFraction, IncreasingPenaltyFactor,
                    UseFunctionIter, StopWhenBalanced, KeepHistory)
    starts[[s]] <- data.frame(start = s, tot.withinss = fit$tot.withinss,
                              maxdiff = fit$maxdiff, balanced = fit$balanced,
                              converged = fit$converged, iter = fit$iter,
                              termination = fit$termination,
                              stringsAsFactors = FALSE)
    # Feasible runs outrank infeasible ones. Among feasible runs select SSE;
    # among infeasible runs select size difference first, then SSE.
    take <- is.null(best)
    if (!take) {
      take <- (fit$balanced && !best$balanced) ||
        (fit$balanced && best$balanced &&
           fit$tot.withinss < best$tot.withinss) ||
        (!fit$balanced && !best$balanced &&
           (fit$maxdiff < best$maxdiff ||
              (fit$maxdiff == best$maxdiff &&
                 fit$tot.withinss < best$tot.withinss)))
    }
    if (take) {
      best <- fit
      best.start <- s
    }
    if (Verbose) {
      message(sprintf("BKM+ start %d/%d: SSE=%.8g, size difference=%d, %s",
                      s, Nstart, fit$tot.withinss, fit$maxdiff,
                      fit$termination))
    }
  }

  # Preserve both the input-row order of memberships and the native cluster
  # numbering produced by the selected run. Cls must use every label 1:ClusterNo.
  best$Cls <- as.numeric(best$Cls)
  if (length(best$Cls) != n || any(!is.finite(best$Cls)) ||
      any(best$Cls != floor(best$Cls)) ||
      any(best$Cls < 1 | best$Cls > k) ||
      any(tabulate(as.integer(best$Cls), nbins = k) == 0L)) {
    stop("Internal error: Cls must contain every cluster label from 1 to ClusterNo.",
         call. = FALSE)
  }
  names(best$Cls) <- if (is.null(rownames(x))) {
    as.character(seq_len(n))
  } else rownames(x)
  rownames(best$centers) <- rownames(best$initial.centers) <- as.character(seq_len(k))
  colnames(best$centers) <- colnames(best$initial.centers) <- colnames(x)
  names(best$size) <- names(best$withinss) <- as.character(seq_len(k))
  best$MSE <- best$tot.withinss / n
  best$nstart <- Nstart
  best$best.start <- best.start
  best$starts <- do.call(rbind, starts)
  best$maxdiff.requested <- MaxDiff
  best$parameters <- list(ClusterNo = k, MaxDiff = MaxDiff, IterMax = IterMax,
                         Switch = Switch, Nstart = Nstart, Seed = Seed,
                         PartlyRemainingFraction = PartlyRemainingFraction,
                         IncreasingPenaltyFactor = IncreasingPenaltyFactor,
                         UseFunctionIter = UseFunctionIter,
                         StopWhenBalanced = StopWhenBalanced)
  best$call <- Call
  if (!best$converged || !best$balanced) {
    warning(sprintf(paste0("BalancedKmeansClustering: selected run ended with '%s'; ",
                           "size difference is %d (requested <= %d). ",
                           "Inspect Object$balanced and Object$converged; ",
                           "consider increasing IterMax or Nstart."),
                    best$termination, best$maxdiff, MaxDiff), call. = FALSE)
  }
  if (PlotIt) {
    if (n < 3L) {
      warning("PlotIt requires at least three observations; skipping the MDS plot.",
              call. = FALSE)
    } else {
      ClusterPlotMDS(x, best$Cls)
    }
  }
  list(Cls = best$Cls, Object = best, Centroids = best$centers)
}

.bkm_integer <- function(x, name, lower, upper = .Machine$integer.max) {
  ###############################################################################
  # y=.bkm_integer(x,name,lower,upper)
  # Validates one integer-valued control parameter.
  #
  # INPUT
  # x       Scalar value to validate.
  # name    Character name used in error messages.
  # lower   Inclusive lower bound.
  #
  # OPTIONAL
  # upper   Inclusive upper bound. Default: .Machine$integer.max.
  #
  # OUTPUT
  # y       Integer scalar within the requested bounds.
  #
  if (!is.numeric(x) || is.complex(x) || length(x) != 1L || !is.finite(x) ||
      x != floor(x) || x < lower || x > upper) {
    stop(sprintf("%s must be an integer between %s and %s.", name, lower, upper),
         call. = FALSE)
  }
  as.integer(x)
}

.bkm_data <- function(x, name, RejectDistances = TRUE) {
  ###############################################################################
  # Data=.bkm_data(x,name,RejectDistances)
  # Validates feature data and converts it to a double matrix.
  #
  # INPUT
  # x                 Numeric matrix, all-numeric data.frame, or numeric vector.
  # name              Character name used in error messages.
  #
  # OPTIONAL
  # RejectDistances   Logical; reject dist objects and recognized distance matrices.
  #                   Default: TRUE.
  #
  # OUTPUT
  # Data              Non-empty finite numeric matrix. If RejectDistances is
  #                   TRUE, distance representations are rejected.
  #
  if (RejectDistances && inherits(x, "dist")) {
    stop(sprintf("%s must contain feature values, not a 'dist' object.", name),
         call. = FALSE)
  }
  if (RejectDistances && (is.matrix(x) || is.data.frame(x))) {
    candidate <- as.matrix(x)
    if (is.numeric(candidate) && !is.complex(candidate) &&
        nrow(candidate) == ncol(candidate) && nrow(candidate) >= 2L &&
        !anyNA(candidate) && all(is.finite(candidate))) {
      tolerance <- 100 * .Machine$double.eps * max(1, max(abs(candidate)))
      if (max(abs(candidate - t(candidate))) <= tolerance &&
          max(abs(diag(candidate))) <= tolerance &&
          min(candidate) >= -tolerance) {
        stop(sprintf("%s must contain feature values, not a distance matrix.", name),
             call. = FALSE)
      }
    }
  }
  if (is.data.frame(x)) {
    if (!all(vapply(x, function(z) is.numeric(z) && !is.complex(z), logical(1)))) {
      stop(sprintf("Every column of %s must be real numeric; encode categorical data explicitly.",
                   name), call. = FALSE)
    }
    x <- as.matrix(x)
  } else if (is.numeric(x) && is.null(dim(x)) && !is.complex(x)) {
    x <- matrix(x, ncol = 1L, dimnames = list(names(x), NULL))
  }
  if (!is.matrix(x) || !is.numeric(x) || is.complex(x) ||
      nrow(x) < 1L || ncol(x) < 1L || any(!is.finite(x))) {
    stop(sprintf("%s must be a nonempty real numeric matrix, data frame or vector without NA/NaN/Inf.",
                 name), call. = FALSE)
  }
  storage.mode(x) <- "double"
  x
}

.bkm_finite <- function(x) {
  ###############################################################################
  # y=.bkm_finite(x)
  # Stops on non-finite intermediate values.
  #
  # INPUT
  # x       Numeric object to validate.
  #
  # OUTPUT
  # y       Unchanged finite numeric object.
  #
  if (any(!is.finite(x))) {
    stop(paste0("Numerical overflow in balanced k-means. ",
                "Center and rescale the feature data, and check any supplied Centers."),
         call. = FALSE)
  }
  x
}

.bkm_dist <- function(x, center) {
  ###############################################################################
  # SquaredDistances=.bkm_dist(x,center)
  # Computes squared Euclidean distances from data rows to one center.
  #
  # INPUT
  # x                   [1:n,1:d] numerical matrix.
  # center              [1:d] numerical centroid vector.
  #
  # OUTPUT
  # SquaredDistances    [1:n] numerical vector.
  #
  .bkm_finite(rowSums(sweep(x, 2L, center, FUN = "-") ^ 2))
}

.bkm_errors <- function(x, Cls, centers) {
  ###############################################################################
  # SquaredErrors=.bkm_errors(x,Cls,centers)
  # Computes each observation's squared distance to its assigned centroid.
  #
  # INPUT
  # x               [1:n,1:d] numerical matrix.
  # Cls             [1:n] numerical vector with values in 1:nrow(centers).
  # centers         [1:k,1:d] numerical centroid matrix.
  #
  # OUTPUT
  # SquaredErrors   [1:n] numerical vector.
  #
  .bkm_finite(rowSums((x - centers[Cls, , drop = FALSE]) ^ 2))
}

.bkm_centers <- function(x, Cls, k) {
  ###############################################################################
  # centers=.bkm_centers(x,Cls,k)
  # Recomputes arithmetic centroids for all clusters.
  #
  # INPUT
  # x         [1:n,1:d] numerical matrix.
  # Cls       [1:n] numerical vector with values in 1:k.
  # k         Number of clusters.
  #
  # OUTPUT
  # centers   [1:k,1:d] numerical centroid matrix.
  #
  out <- matrix(0, k, ncol(x))
  for (j in seq_len(k)) out[j, ] <- colMeans(x[Cls == j, , drop = FALSE])
  .bkm_finite(out)
}

.bkm_run <- function(x, initial, maxdiff, itermax, switchmax, fraction,
                     factor, use.function, stop.balanced, keep.history) {
  ###############################################################################
  # V=.bkm_run(x,initial,maxdiff,itermax,switchmax,fraction,factor,
  #            use.function,stop.balanced,keep.history)
  # Executes one initialized BKM+ run and optional switch refinement.
  #
  # INPUT
  # x               [1:n,1:d] numerical feature matrix.
  # initial         [1:k,1:d] initial centroid matrix.
  # maxdiff         Maximum accepted cluster-size difference.
  # itermax         Maximum balancing iterations.
  # switchmax       Maximum switch-refinement passes.
  # fraction        Fractional old-cluster term in the penalty gap.
  # factor          Constant penalty multiplier when use.function is FALSE.
  # use.function    Logical; use the iteration-dependent multiplier.
  # stop.balanced   Logical; permit early stop after non-improving feasibility.
  # keep.history    Logical; retain iteration histories.
  #
  # OUTPUT
  # V               List containing Cls, centers and run diagnostics.
  #
  n <- nrow(x)
  k <- nrow(initial)
  centers <- initial
  Cls <- numeric(n)
  nearest <- rep(Inf, n)
  # Fixed-center first assignment, using the first cluster in exact ties.
  for (j in seq_len(k)) {
    distances <- .bkm_dist(x, centers[j, ])
    select <- distances < nearest
    Cls[select] <- j
    nearest[select] <- distances[select]
  }
  sizes <- tabulate(Cls, nbins = k)
  sums <- matrix(0, k, ncol(x))
  grouped <- rowsum(x, group = Cls, reorder = TRUE)
  sums[as.integer(rownames(grouped)), ] <- grouped
  .bkm_finite(sums)

  # Duplicate rows or user-supplied centers may produce empty initial clusters.
  # Seed each empty cluster with a farthest point from a nonsingleton donor.
  # This safeguard is deliberately limited to initialization, not a substitute
  # for the adaptive-penalty balancing algorithm.
  repaired <- sum(sizes == 0L)
  for (j in which(sizes == 0L)) {
    errors <- .bkm_errors(x, Cls, centers)
    errors[sizes[Cls] <= 1L] <- -Inf
    i <- which.max(errors)
    old <- Cls[i]
    sizes[old] <- sizes[old] - 1L
    sizes[j] <- 1L
    sums[old, ] <- sums[old, ] - x[i, ]
    sums[j, ] <- x[i, ]
    centers[old, ] <- sums[old, ] / sizes[old]
    centers[j, ] <- x[i, ]
    Cls[i] <- j
  }
  if (k == 1L) centers <- .bkm_centers(x, Cls, k)

  penalty <- 0
  next.penalty <- Inf
  best.sse <- Inf
  best.Cls <- NULL
  best.penalty <- 0
  best.iter <- 1L
  converged <- FALSE
  termination <- "iteration_limit"
  history <- if (keep.history) list() else NULL
  for (iter in seq_len(itermax)) {
    moved <- 0L
    if (iter > 1L && k > 1L) {
      for (i in seq_len(n)) {
        old <- Cls[i]
        if (sizes[old] == 1L) next  # Never remove a singleton.
        point <- x[i, ]
        sizes[old] <- sizes[old] - 1L
        sums[old, ] <- sums[old, ] - point
        centers[old, ] <- sums[old, ] / sizes[old]
        distances <- .bkm_dist(centers, point)
        # The point is completely absent from the old coordinate sum but
        # fractionally present in its size penalty, exactly as in the reference.
        # Subtract integer sizes BEFORE adding the fraction to avoid cancellation.
        gap <- (sizes[old] - sizes) + fraction
        delta <- distances - distances[old]
        smaller <- gap > 0
        other <- seq_len(k) != old
        threshold <- delta / gap
        waiting <- other & smaller & threshold > penalty
        if (any(waiting)) next.penalty <- min(next.penalty, threshold[waiting])

        # Smaller clusters are admissible at equality; larger ones require a
        # strict improvement. The threshold test also avoids scoring candidates
        # that cannot improve the penalized assignment cost.
        eligible <- other & ((smaller & threshold <= penalty) |
                               (!smaller & penalty < threshold))
        target <- old
        if (any(eligible)) {
          candidates <- which(eligible)
          # Subtracting a common size term preserves the argmin and reduces
          # overflow risk compared with distance + penalty * absolute size.
          cost <- distances[candidates] +
            penalty * (sizes[candidates] - min(sizes[candidates]))
          cost <- .bkm_finite(cost)
          # In an exact cost tie, favor the smaller target, then its index.
          # The reference's index-only tie rule can cycle indefinitely at
          # zero penalty on duplicated/all-identical observations.
          tied <- candidates[cost == min(cost)]
          target <- tied[which.min(sizes[tied])]
        }
        sizes[target] <- sizes[target] + 1L
        sums[target, ] <- sums[target, ] + point
        centers[target, ] <- sums[target, ] / sizes[target]
        Cls[i] <- target
        if (target != old) moved <- moved + 1L
      }
    }
    .bkm_finite(centers)
    sse <- .bkm_finite(sum(.bkm_errors(x, Cls, centers)))
    difference <- max(sizes) - min(sizes)
    feasible <- difference <= maxdiff
    hold <- FALSE
    terminate <- FALSE
    if (feasible && sse < best.sse) {
      best.sse <- sse
      best.Cls <- Cls
      best.penalty <- penalty
      best.iter <- iter
      hold <- TRUE
    } else if (feasible && (stop.balanced || difference <= 1L)) {
      converged <- TRUE
      termination <- "balanced_no_improvement"
      terminate <- TRUE
    }
    if (k == 1L) {
      converged <- TRUE
      termination <- "single_cluster"
      terminate <- TRUE
    }
    if (keep.history) {
      history[[iter]] <- data.frame(iter = iter, penalty = penalty,
                                    working.sse = sse, maxdiff = difference,
                                    balanced = feasible, moved = moved)
    }
    if (terminate || iter == itermax) break

    # The first assignment and the first sequential sweep both use zero penalty.
    # Thresholds are retained while a feasible solution improves, as upstream.
    if (iter > 1L && !hold) {
      if (is.finite(next.penalty)) {
        step <- iter - 1L
        multiplier <- if (use.function) {
          if (step > 100L) 1.01 else 1.1009 - 0.0009 * step
        } else factor
        proposed <- multiplier * next.penalty
        if (!is.finite(proposed) || proposed <= penalty) {
          termination <- "penalty_numerical_limit"
          break
        }
        penalty <- proposed
        next.penalty <- Inf
      } else if (!is.null(best.Cls)) {
        converged <- TRUE
        termination <- "no_further_penalty"
        break
      } else if (moved == 0L) {
        termination <- "penalty_stalled"
        break
      }
      # If no larger finite threshold exists but points still move, retain the
      # finite penalty for another sweep rather than multiplying infinity.
    }
  }

  # Unlike the upstream iteration-limit path, never discard a saved feasible
  # solution. Always rebuild centroids from the returned classification.
  if (!is.null(best.Cls)) {
    Cls <- best.Cls
  } else {
    best.penalty <- penalty
    best.iter <- iter
  }
  centers <- .bkm_centers(x, Cls, k)
  before.switch <- .bkm_finite(sum(.bkm_errors(x, Cls, centers)))
  refined <- .bkm_switch(x, Cls, centers, switchmax, keep.history)
  Cls <- refined$Cls
  centers <- refined$centers
  sizes <- tabulate(Cls, nbins = k)
  errors <- .bkm_errors(x, Cls, centers)
  withinss <- vapply(seq_len(k), function(j) sum(errors[Cls == j]), numeric(1))
  sse <- .bkm_finite(sum(withinss))
  difference <- max(sizes) - min(sizes)
  list(Cls = Cls, centers = centers, size = sizes,
       withinss = withinss, tot.withinss = sse,
       iter = iter, best.iter = best.iter, penalty = best.penalty,
       last.penalty = penalty, maxdiff = difference,
       balanced = difference <= maxdiff, hard.balanced = difference <= 1L,
       converged = converged, termination = termination,
       initial.centers = initial, initial.empty.clusters = repaired,
       sse.before.switch = before.switch,
       switch.iter = refined$iter, switches = refined$switches,
       switch.converged = refined$converged,
       history = if (keep.history) do.call(rbind, history) else NULL,
       switch.history = refined$history)
}

.bkm_switch <- function(x, Cls, centers, limit, keep.history) {
  ###############################################################################
  # V=.bkm_switch(x,Cls,centers,limit,keep.history)
  # Refines SSE by exchanging observation pairs while preserving cluster sizes.
  #
  # INPUT
  # x              [1:n,1:d] numerical feature matrix.
  # Cls            [1:n] numerical vector with values in 1:nrow(centers).
  # centers        [1:k,1:d] numerical centroid matrix.
  # limit          Maximum number of switch passes.
  # keep.history   Logical; retain pass-wise diagnostics.
  #
  # OUTPUT
  # V              Flat list with Cls, centers, iter, switches, converged,
  #                and history.
  #
  k <- nrow(centers)
  passes <- 0L
  switches <- 0
  converged <- if (limit == 0L) NA else TRUE
  history <- if (keep.history) list() else NULL
  if (limit > 0L && k > 1L) {
    for (pass in seq_len(limit)) {
      count <- 0
      for (a in seq_len(k - 1L)) {
        for (b in seq.int(a + 1L, k)) {
          ia <- which(Cls == a)
          ib <- which(Cls == b)
          xa <- x[ia, , drop = FALSE]
          xb <- x[ib, , drop = FALSE]
          da <- .bkm_dist(xa, centers[b, ]) - .bkm_dist(xa, centers[a, ])
          db <- .bkm_dist(xb, centers[a, ]) - .bkm_dist(xb, centers[b, ])
          oa <- order(da, ia)
          ob <- order(db, ib)
          pairs <- seq_len(min(length(ia), length(ib)))
          improving <- da[oa[pairs]] + db[ob[pairs]] < 0
          number <- sum(cumprod(improving))
          if (number > 0L) {
            take <- seq_len(number)
            Cls[ia[oa[take]]] <- b
            Cls[ib[ob[take]]] <- a
            # Centroids are fixed within a cluster-pair optimization, then
            # recomputed before the next pair. Cardinalities never change.
            centers[a, ] <- colMeans(x[Cls == a, , drop = FALSE])
            centers[b, ] <- colMeans(x[Cls == b, , drop = FALSE])
            count <- count + number
          }
        }
      }
      passes <- pass
      switches <- switches + count
      converged <- count == 0L
      if (keep.history) {
        history[[pass]] <- data.frame(iter = pass, switches = count,
          sse = .bkm_finite(sum(.bkm_errors(x, Cls, centers))))
      }
      if (converged) break
    }
  }
  return(list(Cls = Cls, centers = centers, iter = passes,
       switches = switches, converged = converged,
       history = if (keep.history && length(history)) do.call(rbind, history) else NULL))
}
