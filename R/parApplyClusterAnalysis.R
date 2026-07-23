parApplyClusterAnalysis = function(DataOrDistances, FUN,
                                    NumberOfTrials = 1:100,
                                    ClusterNo = NULL, WorkersOrNo,
                                    SocketType = "PSOCK", SetSeed = TRUE, ...) {
  # data(Hepta)
  # Distance=as.matrix(parallelDist::parallelDist(Hepta$Data))
  # out=parApplyClusterAnalysis(DataOrDistances = Distance,FUN=APclustering,ClusterNo = 7)

  #
  # Repeatedly applies a clustering function to data or distances, optionally
  # using parallel workers. Supports a single input or a list of inputs.
  #
  # INPUT
  # DataOrDistances  Data matrix, data.frame, distance object, or a list of such
  #                  inputs. A list is interpreted as a collection of datasets or
  #                  distance objects and processed sequentially at the outer
  #                  level.
  #
  # FUN              Function or character string naming a clustering function.
  #                  The function should return a list containing a unique element
  #                  named Cls if a clustering matrix is required.
  #
  # OPTIONAL
  # NumberOfTrials   Positive integer count or vector of positive integer trial
  #                  identifiers. A scalar n generates trials seq_len(n).
  #                  Default: 1:100.
  #
  # ClusterNo        Number of clusters supplied to FUN. For a list of inputs,
  #                  this may be a scalar or one value per input. NULL or NA means
  #                  that ClusterNo is not supplied to FUN.
  #
  # WorkersOrNo      Missing, NULL, a positive integer worker count, or an existing
  #                  parallel cluster object.
  #                  - missing: use detectCores() - 1, with at least one worker
  #                  - NULL: execute serially
  #                  - integer: create and subsequently stop a worker cluster
  #                  - cluster object: use it without stopping it
  #
  # SocketType       Character scalar passed as type to parallel::makeCluster().
  #                  Default: "PSOCK".
  #
  # SetSeed          Logical scalar. If TRUE, trial i uses seed 1000 + i.
  #                  If FALSE, deterministic trial seeds are not assigned.
  #                  Default: TRUE.
  #
  # ...              Further arguments forwarded to FUN. Arguments supplied here
  #                  may not duplicate DataOrDistances or ClusterNo as supplied by
  #                  the wrapper.
  #
  # OUTPUT
  # For a single input, a list with:
  # Cls_Matrix       Matrix whose columns contain Cls returned by each trial, or
  #                  NULL if at least one result has no unique Cls element.
  # ComputationTime  Named numeric vector with elapsed time per trial.
  # Seeds            Named integer vector of seeds, or NULL when SetSeed = FALSE.
  # CAobjects        Included if Cls_Matrix cannot be constructed.
  #
  # For a list of inputs, returns a list containing one such result per input.
  #
  # DETAILS
  # The input argument of FUN is identified from its formal arguments. Preferred
  # names are DataOrDistances, Data, Distances, Distance, and Dissimilarities.
  # Unsupported named arguments are removed when FUN has no ... argument.
  #
  # Parallel execution uses parallel::parLapply(). PSOCK is the portable default
  # and is supported on Windows, macOS, and Unix-like systems.
  #
  # author: Michael Thrun

  caller = "parApplyClusterAnalysis"
  workers_missing = missing(WorkersOrNo)
  SetSeed = .fcps_validate_set_seed(SetSeed)
  trial_ids = .fcps_trial_ids(NumberOfTrials)
  fun_object = match.fun(FUN)
  dots = list(...)

  is_collection = .fcps_is_input_collection(DataOrDistances)
  if (is_collection) {
    n_inputs = length(DataOrDistances)
    if (n_inputs == 0L) {
      return(vector("list", 0L))
    }
    cluster_numbers = .fcps_expand_cluster_no(ClusterNo, n_inputs, caller)
  } else {
    cluster_number = .fcps_normalize_cluster_no(ClusterNo)
  }

  if (workers_missing) {
    WorkersOrNo = .fcps_default_workers()
  }
  backend = .fcps_prepare_backend(WorkersOrNo, SocketType)
  if (isTRUE(backend$owned)) {
    on.exit(
      try(parallel::stopCluster(backend$cluster), silent = TRUE),
      add = TRUE
    )
  }

  if (is.null(backend$cluster)) {
    message("Compute benchmarking of clustering method serially")
  } else if (isTRUE(backend$owned)) {
    message("Compute benchmarking of clustering method with a new worker cluster")
  } else {
    message("Compute benchmarking of clustering method with the supplied worker cluster")
  }

  run_single = function(input, current_cluster_no) {
    worker_args = .fcps_merge_worker_args(
      list(
        fun = fun_object,
        DataOrDistances = input,
        ClusterNo = current_cluster_no,
        SetSeed = SetSeed
      ),
      dots
    )
    out = .fcps_apply_trials(
      cl = backend$cluster,
      trial_ids = trial_ids,
      worker_fun = cluster_analysis_fun,
      worker_args = worker_args
    )
    .fcps_aggregate_trials(
      out = out,
      SetSeed = SetSeed,
      caller = caller,
      include_objects = FALSE
    )
  }

  if (!is_collection) {
    return(run_single(DataOrDistances, cluster_number))
  }

  input_names = names(DataOrDistances)
  benchmarking = vector("list", n_inputs)
  for (i in seq_len(n_inputs)) {
    input_label = .fcps_input_label(input_names, i)
    message(sprintf("Computing dataset %s (%d of %d)", input_label, i, n_inputs))
    benchmarking[[i]] = run_single(
      DataOrDistances[[i]],
      cluster_numbers[[i]]
    )
  }
  names(benchmarking) = input_names
  benchmarking
}
