cluster_analysis_fun=function(i,fun,DataOrDistance,ClusterNo,SetSeed=TRUE,...){
  #example
  # data(Hepta)
  # Distance=as.matrix(parallelDist::parallelDist(Hepta$Data))
  # out=cluster_analysis_fun(i = 1,fun = APclustering,DataOrDistance = Distance,ClusterNo = 7)
  if(isTRUE(SetSeed)){
    seedno=1000+i
    set.seed(seed = seedno)
    nndelta=paste0('Seed_',seedno)
  }else{
    nndelta=paste0(i)
    set.seed(seed = NULL)
  }
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
  delta=as.vector(as.numeric(difftime(past,prior,units = 'secs')))
  names(delta)=nndelta
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