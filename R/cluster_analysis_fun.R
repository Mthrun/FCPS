cluster_analysis_fun=function(i,fun,DataOrDistances,ClusterNo,SetSeed=TRUE,...){
  #example
  # data(Hepta)
  # Distance=as.matrix(parallelDist::parallelDist(Hepta$Data))
  # out=cluster_analysis_fun(i = 1,fun = APclustering,DataOrDistances = Distance,ClusterNo = 7)
  if(isTRUE(SetSeed)){
    seedno=1000+i
    set.seed(seed = seedno)
    nndelta=paste0('Seed_',seedno)
  }else{
    nndelta=paste0(i)
    set.seed(seed = NULL)
  }
  prior=Sys.time()
  string=names(formals(fun))
  
  if(is.null(ClusterNo)){
    if (isSymmetric(unname(DataOrDistances))) {
      object=R.utils::doCall(fun, args=list(DataOrDistances=DataOrDistances,...),.ignoreUnusedArgs=TRUE)
    }else{
      if(string[1]=="Data")
        object=R.utils::doCall(fun, args=list(Data=DataOrDistances,...),.ignoreUnusedArgs=TRUE)
      else
        object=R.utils::doCall(fun, args=list(DataOrDistances=DataOrDistances,...),.ignoreUnusedArgs=TRUE)
    }
    #object=fun(DataOrDistances,...)
  }else{
    if(isSymmetric(unname(DataOrDistances))) {
      object=R.utils::doCall(fun,  args=list(DataOrDistancess=DataOrDistances,ClusterNo=ClusterNo,...),.ignoreUnusedArgs=TRUE)
    }else{
      if(string[1]=="Data")
        object=R.utils::doCall(fun,  args=list(Data=DataOrDistances,ClusterNo=ClusterNo,...),.ignoreUnusedArgs=TRUE)
      else
        object=R.utils::doCall(fun,  args=list(DataOrDistances=DataOrDistances,ClusterNo=ClusterNo,...),.ignoreUnusedArgs=TRUE)
    }
    #object=fun(DataOrDistances,ClusterNo,...)
  }
  past=Sys.time()
  delta=as.vector(as.numeric(difftime(past,prior,units = 'secs')))
  names(delta)=nndelta
  nn=names(object)
  ind=which(nn=='Cls')
  if(length(ind)==1){
    Liste=list(Cls=object[[ind]],ComputationTime=delta,Seed=seedno)
  }else{
    warning('"Cls" object could not be found. Everything available is returned.')
    Liste=list(Cls=object,ComputationTime=delta,Seed=seedno)
  }
  return(Liste)
}#end help_fun