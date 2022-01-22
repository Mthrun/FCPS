ClusterDaviesBouldinIndex=function(Cls,Data,...){
  
  if (!requireNamespace('clusterSim',quietly = TRUE)){
    message(
      'Subordinate package (clusterSim) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        DaviesBouldinIndex = NaN,
        Object = "Subordinate package (clusterSim) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }else{
    if(missing(Cls))
      stop("ClusterDaviesBouldinIndex: Cls is missing")
    if(missing(Data))
      stop("ClusterDaviesBouldinIndex: Data is missing")
    
    if(length(Cls)!=nrow(Data))
      stop("ClusterDaviesBouldinIndex: Number of rows in 'Data' does not equal length of 'Cls'")
      
    if(length(unique(Cls))==1){
      warning("ClusterDaviesBouldinIndex: 'Cls' has only one cluster stored.")
    }
    
    out=clusterSim::index.DB(Data,cl = Cls,...)
    return(list(DaviesBouldinIndex=out$DB,Object=out))
  }
}