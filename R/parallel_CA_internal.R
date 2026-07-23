# Internal helpers for the parallel clustering-analysis wrappers.
# These functions deliberately use only base R and the recommended package
# 'parallel', so the wrappers no longer depend on R.utils::doCall().

.fcps_is_input_collection <- function(x) {
  is.list(x) && !is.data.frame(x)
}

.fcps_validate_set_seed <- function(SetSeed) {
  if (!is.logical(SetSeed) || length(SetSeed) != 1L || is.na(SetSeed)) {
    stop("'SetSeed' must be exactly TRUE or FALSE.", call. = FALSE)
  }
  SetSeed
}

.fcps_trial_ids <- function(NumberOfTrials) {
  if (!is.numeric(NumberOfTrials) || length(NumberOfTrials) == 0L ||
      anyNA(NumberOfTrials) || any(!is.finite(NumberOfTrials)) ||
      any(NumberOfTrials < 1) || any(NumberOfTrials != floor(NumberOfTrials))) {
    stop(
      "'NumberOfTrials' must be a positive integer count or a vector of positive integer trial IDs.",
      call. = FALSE
    )
  }

  max_trial <- .Machine$integer.max - 1000L
  if (any(NumberOfTrials > max_trial)) {
    stop("Trial IDs are too large to construct valid integer seeds.", call. = FALSE)
  }

  if (length(NumberOfTrials) == 1L) {
    seq_len(as.integer(NumberOfTrials))
  } else {
    as.integer(NumberOfTrials)
  }
}

.fcps_normalize_cluster_no <- function(ClusterNo, argument = "ClusterNo") {
  if (is.null(ClusterNo) || length(ClusterNo) == 0L) {
    return(NULL)
  }
  if (length(ClusterNo) != 1L) {
    stop(sprintf("'%s' must have length one for a single dataset.", argument), call. = FALSE)
  }
  if (isTRUE(is.na(ClusterNo))) {
    return(NULL)
  }
  ClusterNo
}

.fcps_expand_cluster_no <- function(ClusterNo, n, caller) {
  if (n == 0L) {
    return(vector("list", 0L))
  }
  if (is.null(ClusterNo) || length(ClusterNo) == 0L) {
    return(rep(list(NULL), n))
  }

  values <- if (is.list(ClusterNo)) ClusterNo else as.list(ClusterNo)

  if (length(values) == 1L) {
    values <- rep(values, n)
  } else if (length(values) < n) {
    warning(
      sprintf(
        "%s: 'ClusterNo' has length %d for %d inputs; extending it with the first value.",
        caller, length(values), n
      ),
      call. = FALSE
    )
    values <- c(values, rep(values[1L], n - length(values)))
  } else if (length(values) > n) {
    warning(
      sprintf(
        "%s: 'ClusterNo' has length %d for %d inputs; ignoring trailing values.",
        caller, length(values), n
      ),
      call. = FALSE
    )
    values <- values[seq_len(n)]
  }

  lapply(
    seq_len(n),
    function(i) .fcps_normalize_cluster_no(values[[i]], argument = sprintf("ClusterNo[[%d]]", i))
  )
}

.fcps_default_workers <- function() {
  if (!requireNamespace("parallel", quietly = TRUE)) {
    return(NULL)
  }

  cores <- suppressWarnings(parallel::detectCores(logical = TRUE))
  if (length(cores) != 1L || is.na(cores) || !is.finite(cores) || cores <= 1L) {
    return(1L)
  }
  max(1L, as.integer(cores) - 1L)
}

.fcps_prepare_backend <- function(WorkersOrNo, SocketType) {
  if (is.null(WorkersOrNo)) {
    return(list(cluster = NULL, owned = FALSE))
  }

  if (!requireNamespace("parallel", quietly = TRUE)) {
    warning("Package 'parallel' is unavailable; using serial execution.", call. = FALSE)
    return(list(cluster = NULL, owned = FALSE))
  }

  if (inherits(WorkersOrNo, "cluster")) {
    return(list(cluster = WorkersOrNo, owned = FALSE))
  }

  if (!is.numeric(WorkersOrNo) || length(WorkersOrNo) != 1L ||
      is.na(WorkersOrNo) || !is.finite(WorkersOrNo) ||
      WorkersOrNo < 1 || WorkersOrNo != floor(WorkersOrNo)) {
    stop(
      "'WorkersOrNo' must be NULL, a positive integer worker count, or a parallel cluster object.",
      call. = FALSE
    )
  }

  if (!is.character(SocketType) || length(SocketType) != 1L || is.na(SocketType) ||
      !nzchar(SocketType)) {
    stop("'SocketType' must be a non-empty character string.", call. = FALSE)
  }

  cl <- parallel::makeCluster(as.integer(WorkersOrNo), type = SocketType)
  list(cluster = cl, owned = TRUE)
}

.fcps_merge_worker_args <- function(base_args, dots) {
  if (length(dots) == 0L) {
    return(base_args)
  }

  dot_names <- names(dots)
  if (is.null(dot_names)) {
    dot_names <- rep("", length(dots))
  }
  duplicate_names <- nzchar(dot_names) & dot_names %in% names(base_args)
  if (any(duplicate_names)) {
    stop(
      sprintf(
        "Arguments in '...' may not redefine wrapper arguments: %s.",
        paste(unique(dot_names[duplicate_names]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  c(base_args, dots)
}

.fcps_apply_trials <- function(cl, trial_ids, worker_fun, worker_args) {
  # Use a base-environment closure so a PSOCK worker does not serialize this
  # helper's call frame (which also contains the cluster connection object).
  invoke_worker <- function(i, worker_fun, worker_args) {
    do.call(worker_fun, c(list(i = i), worker_args))
  }
  environment(invoke_worker) <- baseenv()

  if (is.null(cl)) {
    lapply(
      trial_ids,
      invoke_worker,
      worker_fun = worker_fun,
      worker_args = worker_args
    )
  } else {
    parallel::parLapply(
      cl = cl,
      X = trial_ids,
      fun = invoke_worker,
      worker_fun = worker_fun,
      worker_args = worker_args
    )
  }
}

.fcps_aggregate_trials <- function(out, SetSeed, caller, include_objects = FALSE) {
  if (length(out) == 0L || !all(vapply(out, is.list, logical(1L)))) {
    stop(sprintf("%s: invalid worker result.", caller), call. = FALSE)
  }

  times <- vapply(
    out,
    function(x) {
      value <- x$ComputationTime
      if (!is.numeric(value) || length(value) != 1L) {
        stop("A worker returned an invalid computation time.", call. = FALSE)
      }
      as.numeric(value)
    },
    numeric(1L)
  )

  labels <- vapply(
    seq_along(out),
    function(i) {
      nm <- names(out[[i]]$ComputationTime)
      if (length(nm) == 1L && nzchar(nm)) nm else as.character(i)
    },
    character(1L)
  )
  names(times) <- labels

  seeds <- NULL
  if (isTRUE(SetSeed)) {
    seeds <- vapply(
      out,
      function(x) {
        if (!is.numeric(x$Seed) || length(x$Seed) != 1L || is.na(x$Seed)) {
          stop("A worker returned an invalid seed.", call. = FALSE)
        }
        as.integer(x$Seed)
      },
      integer(1L)
    )
    names(seeds) <- labels
  }

  cls <- lapply(out, function(x) x$Cls)
  has_cls <- !vapply(cls, is.null, logical(1L))
  cls_matrix <- NULL

  if (all(has_cls)) {
    cls_lengths <- vapply(cls, length, integer(1L))
    if (length(unique(cls_lengths)) != 1L) {
      stop(
        sprintf("%s: trials returned 'Cls' vectors of different lengths.", caller),
        call. = FALSE
      )
    }
    cls_matrix <- do.call(cbind, cls)
    colnames(cls_matrix) <- labels
  } else {
    warning(
      sprintf(
        "%s: at least one clustering result has no unique element named 'Cls'; 'Cls_Matrix' is NULL and full results are available in 'CAobjects'.",
        caller
      ),
      call. = FALSE
    )
  }

  objects <- lapply(out, function(x) x$CAs)
  names(objects) <- labels

  result <- list(
    Cls_Matrix = cls_matrix,
    ComputationTime = times,
    Seeds = seeds
  )
  if (isTRUE(include_objects) || !all(has_cls)) {
    result$CAobjects <- objects
  }
  result
}

.fcps_input_label <- function(input_names, i) {
  if (!is.null(input_names) && length(input_names) >= i &&
      !is.na(input_names[[i]]) && nzchar(input_names[[i]])) {
    input_names[[i]]
  } else {
    as.character(i)
  }
}
