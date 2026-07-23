parApplyDistanceBasedCA = function(Distances, FUN, NumberOfTrials = 1:100,
                                    ClusterNo = NULL, WorkersOrNo,
                                    SocketType = "PSOCK", SetSeed = TRUE, ...) {
  # data(Hepta)
  # Distance=as.matrix(parallelDist::parallelDist(Hepta$Data))
  # out=parApplyDistanceBasedCA(Distance,FUN=APclustering,ClusterNo = 7)
  #
  # Repeatedly applies a distance-based clustering function to one distance input
  # or a list of distance inputs, optionally using parallel workers.
  #
  # INPUT
  # Distances        Distance or dissimilarity representation accepted by FUN, or
  #                  a list of such representations.
  #
  # FUN              Function or character string naming a distance-based
  #                  clustering function. FUN should normally return a list with
  #                  a unique element named Cls.
  #
  # OPTIONAL
  # NumberOfTrials   Positive integer count or vector of positive integer trial
  #                  identifiers. Default: 1:100.
  #
  # ClusterNo        Number of clusters passed to FUN. May be scalar or one value
  #                  per input when Distances is a list. NULL or NA omits it.
  #
  # WorkersOrNo      Missing, NULL, positive integer worker count, or an existing
  #                  parallel cluster object.
  #
  # SocketType       Cluster type passed to parallel::makeCluster().
  #                  Default: "PSOCK".
  #
  # SetSeed          Logical scalar. If TRUE, trial i uses seed 1000 + i.
  #                  Default: TRUE.
  #
  # ...              Further arguments forwarded to FUN.
  #
  # OUTPUT
  # For one distance input, a list containing:
  # Cls_Matrix, ComputationTime, Seeds, and CAobjects.
  #
  # For a list of distance inputs, a named list with one result per input.
  #
  # DETAILS
  # The input argument of FUN is selected preferentially from DataOrDistances,
  # Distances, Distance, or Dissimilarities. If none exists, the first inspectable
  # non-dots formal argument is used.
  #
  # author: Michael Thrun and contributors
  
  caller = "parApplyDistanceBasedCA"
  workers_missing = missing(WorkersOrNo)
  SetSeed = .fcps_validate_set_seed(SetSeed)
  trial_ids = .fcps_trial_ids(NumberOfTrials)
  fun_object = match.fun(FUN)
  dots = list(...)

  is_collection = .fcps_is_input_collection(Distances)
  if (is_collection) {
    n_inputs = length(Distances)
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
        Distances = input,
        ClusterNo = current_cluster_no,
        SetSeed = SetSeed
      ),
      dots
    )
    out = .fcps_apply_trials(
      cl = backend$cluster,
      trial_ids = trial_ids,
      worker_fun = CA_dist_fun,
      worker_args = worker_args
    )
    .fcps_aggregate_trials(
      out = out,
      SetSeed = SetSeed,
      caller = caller,
      include_objects = TRUE
    )
  }

  if (!is_collection) {
    return(run_single(Distances, cluster_number))
  }

  input_names = names(Distances)
  benchmarking = vector("list", n_inputs)
  for (i in seq_len(n_inputs)) {
    input_label = .fcps_input_label(input_names, i)
    message(sprintf("Computing dataset %s (%d of %d)", input_label, i, n_inputs))
    benchmarking[[i]] = run_single(Distances[[i]], cluster_numbers[[i]])
  }
  names(benchmarking) = input_names
  benchmarking
}
