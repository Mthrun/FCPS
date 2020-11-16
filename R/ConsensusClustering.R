ConsensusClustering=function(DataOrDistances,ClusterNo=NULL,PlotIt=FALSE,PlotConsensus=FALSE,...){
  # INPUT
  # DataOrDistances[1:n,1:d]     Data set with n observations and d features or distance matrix
  # ClusterNo         Number of clusters to search for
  #
  # OPTIONAL
  # PlotIt            Boolean. Decision to plot or not
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of ConsensusClustering
  #
  # Author: MT, 11/2020
  if (!requireNamespace('ConsensusClusterPlus', quietly = TRUE)) {
    message(
      'Subordinate clustering package (ConsensusClusterPlus of Bioconductor) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate clustering package (ConsensusClusterPlus of Bioconductor) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  if (isSymmetric(unname(DataOrDistances))) {
    DataOrDistances=as.dist(DataOrDistances)
  }else{
    message("Data Matrix max not work in Subordinate package due to strange error.")
  }
  
  if(is.null(ClusterNo)){
    PlotConsensus=NULL
    CA=ConsensusClusterPlus::ConsensusClusterPlus(d = DataOrDistances,plot = PlotConsensus,...)
    return(CA)
  }else{
    CA=ConsensusClusterPlus::ConsensusClusterPlus(d = DataOrDistances,maxK=ClusterNo,plot = PlotConsensus,...)
    Cls=CA[[ClusterNo]]$consensusClass
  }
 
  Cls=ClusterRename(Cls,DataOrDistances)
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  return(list(Cls=Cls,Object=CA))
}
