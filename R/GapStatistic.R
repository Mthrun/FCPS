GapStatistic=function(Data,ClusterNoMax,ClusterFun,...){
  out=cluster::clusGap(x=Data, FUNcluster=ClusterFun, K.max=ClusterNoMax, ...)
  return(out)
}