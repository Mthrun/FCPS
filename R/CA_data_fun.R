CA_data_fun = function(i, fun, Data, ClusterNo = NULL, SetSeed = TRUE, ...) {

  # V=CA_data_fun(i,fun,Data,ClusterNo=2)
  #
  # INTERNAL WORKER
  #
  # Executes one data-based clustering trial, records elapsed time and seed, and
  # extracts the unique element named Cls from the clustering result.
  #
  # INPUT
  # i          Positive integer trial identifier.
  # fun        Function or character string naming a clustering function.
  # Data       Dataset supplied to fun.
  #
  # OPTIONAL
  # ClusterNo  Number of clusters. NULL omits this argument.
  # SetSeed    Logical. If TRUE, uses seed 1000 + i. Default: TRUE.
  # ...        Further arguments forwarded to fun.
  #
  # OUTPUT
  # List with Cls, ComputationTime, Seed, and CAs.
  #
  # INTERNAL
  # Used by parApplyDataBasedCA().
  if (!is.logical(SetSeed) || length(SetSeed) != 1L || is.na(SetSeed)) {
    stop("'SetSeed' must be exactly TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(i) || length(i) != 1L || is.na(i) || !is.finite(i) ||
      i < 1 || i != floor(i) || i > (.Machine$integer.max - 1000L)) {
    stop("'i' must be a positive integer trial ID.", call. = FALSE)
  }

  if (isTRUE(SetSeed)) {
    seedno = 1000L + as.integer(i)
    set.seed(seedno)
    delta_name = paste0("Seed_", seedno)
  } else {
    seedno = NULL
    set.seed(NULL)
    delta_name = as.character(as.integer(i))
  }

  fun_object = match.fun(fun)
  formal_names = names(formals(fun_object))
  if (is.null(formal_names)) {
    stop("'fun' must be an R function with inspectable formal arguments.", call. = FALSE)
  }

  non_dots = formal_names[formal_names != "..."]
  if ("Data" %in% formal_names) {
    input_name = "Data"
  } else if ("DataOrDistances" %in% formal_names) {
    input_name = "DataOrDistances"
  } else if (length(non_dots) > 0L) {
    input_name = non_dots[[1L]]
  } else if ("..." %in% formal_names) {
    input_name = "Data"
  } else {
    stop("Could not identify an input argument in 'fun'.", call. = FALSE)
  }

  call_args = list()
  call_args[[input_name]] = Data
  if (!is.null(ClusterNo)) {
    call_args$ClusterNo = ClusterNo
  }

  dots = list(...)
  if (length(dots) > 0L) {
    dot_names = names(dots)
    if (is.null(dot_names)) {
      dot_names = rep("", length(dots))
    }
    duplicate_names = nzchar(dot_names) & dot_names %in% names(call_args)
    if (any(duplicate_names)) {
      stop(
        sprintf(
          "Arguments in '...' duplicate wrapper-supplied arguments: %s.",
          paste(unique(dot_names[duplicate_names]), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    call_args = c(call_args, dots)
  }

  if (!("..." %in% formal_names)) {
    arg_names = names(call_args)
    if (is.null(arg_names)) {
      arg_names = rep("", length(call_args))
    }
    keep = !nzchar(arg_names) | arg_names %in% formal_names
    call_args = call_args[keep]
  }

  prior = proc.time()[["elapsed"]]
  object = do.call(fun_object, call_args)
  delta = as.numeric(proc.time()[["elapsed"]] - prior)
  names(delta) = delta_name

  cls = NULL
  object_names = names(object)
  if (!is.null(object_names)) {
    cls_index = which(object_names == "Cls")
    if (length(cls_index) == 1L) {
      cls = object[[cls_index]]
    }
  }

  list(
    Cls = cls,
    ComputationTime = delta,
    Seed = seedno,
    CAs = object
  )
}
