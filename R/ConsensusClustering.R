ConsensusClustering=function(Data,ClusterNo=NULL,PlotIt=FALSE,PlotConsensus=FALSE,...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Number of clusters to search for
  #
  # OPTIONAL
  # PlotIt            Boolean. Decision to plot or not
  # PlotConsensus:    FALSE, NULL, "png", "pdf", or "pngBMP".
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of ConsensusClustering
  #
  # Author: MT 07/26


    
    if (!requireNamespace("ConsensusClusterPlus", quietly = TRUE)) {
      warning(paste(
          "Package 'ConsensusClusterPlus' is required.",
          "Install it with BiocManager::install('ConsensusClusterPlus').") )
      return(
        list(
          Cls = rep(1, nrow(Data)),
          Object = "Subordinate clustering package (cluster) is missing  although its imported in ADPclust.
                Please install the package which is defined in 'Suggests'."
        )
      )
    }
    
    Data = as.matrix(Data)
    
    if (!is.numeric(Data)) {
      stop("Data must be numeric.", call. = FALSE)
    }
    
    if (anyNA(Data) || any(!is.finite(Data))) {
      stop("Data must not contain NA, NaN, or infinite values.", call. = FALSE)
    }
    
    if (!is.null(ClusterNo)) {
      if (length(ClusterNo) != 1L || ClusterNo < 2 || ClusterNo %% 1 != 0) {
        stop("ClusterNo must be a single integer greater than 1.", call. = FALSE)
      }
      
      ClusterNo = as.integer(ClusterNo)
    }
    
    # ConsensusClusterPlus expects features in rows
    # and observations in columns.
    CCPData = t(Data)
    
    if (is.null(ClusterNo)) {
      return(
        ConsensusClusterPlus::ConsensusClusterPlus(
          d = CCPData,
          plot = PlotConsensus,
          ...
        )
      )
    }
    
    CA = ConsensusClusterPlus::ConsensusClusterPlus(
      d = CCPData,
      maxK = ClusterNo,
      plot = PlotConsensus,
      ...
    )
    
    Cls = CA[[ClusterNo]]$consensusClass
    
    if (exists("ClusterRename", mode = "function")) {
      Cls = ClusterRename(Cls, Data)
    }
    
    if (isTRUE(PlotIt)) {
      if (!exists("ClusterPlotMDS", mode = "function")) {
        warning("ClusterPlotMDS() was not found.")
      } else {
        ClusterPlotMDS(Data, Cls)
      }
    }
    
    list(
      Cls = Cls,
      Object = CA
    )
  }
  

    