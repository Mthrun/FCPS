parApplyClusterAnalysis=function(DataOrDistance,FUN,NumberOfTrials=1:100,ClusterNo=NULL,NoWorkers,Type="PSOCK",SetSeed=TRUE,...){
  
  #example
  # data(Hepta)
  # Distance=as.matrix(parallelDist::parallelDist(Hepta$Data))
  # out=parApplyClusterAnalysis(DataOrDistance = Distance,FUN=APclustering,ClusterNo = 7)
  requireNamespace('parallel')
  if(missing(NoWorkers))
    NoWorkers <- parallel::detectCores() - 1

  cl=parallel::makeCluster(NoWorkers,type = Type)
  

  
  tryCatch({
    if (isSymmetric(unname(DataOrDistance))) {
      DataOrDistances=DataOrDistance
      out=parallel::parLapply(cl = cl,X = NumberOfTrials,fun = cluster_analysis_fun,FUN,DataOrDistances,ClusterNo,SetSeed,...)
    }else{
      out=parallel::parLapply(cl = cl,X = NumberOfTrials,fun = cluster_analysis_fun,FUN,Data,ClusterNo,SetSeed,...)
    }

  },error=function(e){
    print(e)
    parallel::stopCluster(cl)
  })
  parallel::stopCluster(cl)
  
  Cls_matrix=simplify2array(lapply(out, `[[`, 1),higher = FALSE)
  CompTimeVec=sapply(out, `[[`, 2)
  
  return(list(Cls_Matrix=Cls_matrix,ComputationTime=CompTimeVec))
}
