GapStatistic=function(Data,ClusterNoMax,ClusterFun,...){
  if (!requireNamespace('cluster',quietly = TRUE)) {
    message(
      'Subordinate clustering package (cluster) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate clustering package (cluster) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  out=cluster::clusGap(x=Data, FUNcluster=ClusterFun, K.max=ClusterNoMax, ...)
  return(out)
}
