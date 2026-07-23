parApplyDataBasedCA = function(Data, FUN, NumberOfTrials = 1:100,
                                ClusterNo = NULL, WorkersOrNo,
                                SocketType = "PSOCK", SetSeed = TRUE, ...) {
  # data(Hepta)
  # parApplyDataBasedCA(Data = list(FCPS$Hepta$Data,NaN,FCPS$Hepta$Data),FUN = PAMclustering,NumberOfTrials = 2,ClusterNo = c(7,7,7),WorkersOrNo = 2)
  #
  # Repeatedly applies a data-based clustering function to one dataset or a list
  # of datasets, optionally using parallel workers.
  #
  # INPUT
  # Data             Numeric matrix, data.frame, or list of datasets. A list that
  #                  is not a data.frame is interpreted as a collection of inputs.
  #
  # FUN              Function or character string naming a data-based clustering
  #                  function. FUN should normally return a list with element Cls.
  #
  # OPTIONAL
  # NumberOfTrials   Positive integer count or vector of positive integer trial
  #                  identifiers. A scalar n generates seq_len(n).
  #                  Default: 1:100.
  #
  # ClusterNo        Number of clusters passed to FUN. For multiple datasets, may
  #                  be scalar or provide one value per dataset. NULL or NA omits
  #                  ClusterNo from the call.
  #
  # WorkersOrNo      Missing, NULL, positive integer worker count, or an existing
  #                  parallel cluster object. See parApplyClusterAnalysis().
  #
  # SocketType       Cluster type passed to parallel::makeCluster().
  #                  Default: "PSOCK".
  #
  # SetSeed          Logical scalar. If TRUE, trial i uses seed 1000 + i.
  #                  Default: TRUE.
  #
  # ...              Further arguments forwarded to FUN. Wrapper-provided
  #                  arguments may not be redefined through ....
  #
  # OUTPUT
  # For one dataset, a list with:
  # Cls_Matrix       Matrix of cluster assignments, one column per trial, or NULL
  #                  when a trial does not return a unique Cls element.
  # ComputationTime  Named numeric vector of elapsed trial times.
  # Seeds            Named integer vector, or NULL if SetSeed = FALSE.
  # CAobjects        List containing the complete clustering result for each trial.
  #
  # For multiple datasets, returns a named list of these results. If processing an
  # individual dataset fails, that element is set to NULL and a warning is issued.
  #
  # DETAILS
  # FUN receives Data using a formal argument named Data, DataOrDistances, or the
  # first inspectable non-dots formal argument.
  #
  # author: Michael Thrun
  
  caller = "parApplyDataBasedCA"
  workers_missing = missing(WorkersOrNo)
  SetSeed = .fcps_validate_set_seed(SetSeed)
  trial_ids = .fcps_trial_ids(NumberOfTrials)
  fun_object = match.fun(FUN)
  dots = list(...)

  is_collection = .fcps_is_input_collection(Data)
  if (is_collection) {
    n_inputs = length(Data)
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
        Data = input,
        ClusterNo = current_cluster_no,
        SetSeed = SetSeed
      ),
      dots
    )
    out = .fcps_apply_trials(
      cl = backend$cluster,
      trial_ids = trial_ids,
      worker_fun = CA_data_fun,
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
    return(run_single(Data, cluster_number))
  }

  input_names = names(Data)
  benchmarking = vector("list", n_inputs)
  for (i in seq_len(n_inputs)) {
    input_label = .fcps_input_label(input_names, i)
    message(sprintf("Computing dataset %s (%d of %d)", input_label, i, n_inputs))

    value = tryCatch(
      run_single(Data[[i]], cluster_numbers[[i]]),
      error = function(e) {
        warning(
          sprintf(
            "%s failed for dataset %s: %s",
            caller, input_label, conditionMessage(e)
          ),
          call. = FALSE
        )
        NULL
      }
    )
    benchmarking[i] = list(value)
  }
  names(benchmarking) = input_names
  benchmarking
}
