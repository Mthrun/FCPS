cluster_analysis_fun=function(i,fun,DataOrDistance,ClusterNo,...){

  #example
  # data(Hepta)
  # Distance=as.matrix(parallelDist::parallelDist(Hepta$Data))
  # out=cluster_analysis_fun(i = 1,fun = APclustering,DataOrDistance = Distance,ClusterNo = 7)
  prior=Sys.time()
  if(is.null(ClusterNo)){
    if (isSymmetric(DataOrDistance)) {
      object=R.utils::doCall(fun, args=list(DataOrDistances=DataOrDistance,...),.ignoreUnusedArgs=TRUE)
    }else{
      object=R.utils::doCall(fun, args=list(Data=DataOrDistance,...),.ignoreUnusedArgs=TRUE)
    }
    #object=fun(DataOrDistance,...)
  }else{
    if(isSymmetric(DataOrDistance)) {
      object=R.utils::doCall(fun,  args=list(DataOrDistances=DataOrDistance,ClusterNo=ClusterNo,...),.ignoreUnusedArgs=TRUE)
    }else{
      object=R.utils::doCall(fun,  args=list(Data=DataOrDistance,ClusterNo=ClusterNo,...),.ignoreUnusedArgs=TRUE)
      
    }
    #object=fun(DataOrDistance,ClusterNo,...)
  }
  past=Sys.time()
  delta=difftime(past,prior,units = 'secs')
  nn=names(object)
  ind=which(nn=='Cls')
  if(length(ind)==1){
    Liste=list(Cls=object[[ind]],ComputationTime=delta)
  }else{
    warning('"Cls" object could not be found. Everything available is returned.')
    Liste=list(Cls=object,ComputationTime=delta)
  }
  return(Liste)
}#end help_fun