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
    out=clusterSim::index.DB(Data,cl = Cls,...)
    return(list(DaviesBouldinIndex=out$DB,Object=out))
  }
}