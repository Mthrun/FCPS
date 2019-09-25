parApplyClusterAnalysis=function(DataOrDistance,FUN,NumberOfTrials=1:100,ClusterNo=NULL,NoWorkers,Type="PSOCK",...){
  
  requireNamespace('parallel')
  if(missing(NoWorkers))
    NoWorkers <- parallel::detectCores() - 1

  cl=parallel::makeCluster(NoWorkers,type = Type)

  help_fun=function(i,fun,DataOrDistance,ClusterNo,...){
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
  
  if (isSymmetric(DataOrDistances)) {
    if(!inherits(DataOrDistances,'dist'))
      Input=as.dist(DataOrDistances)
    else
      Input=DataOrDistances
    
    return(HierarchicalClusterDists(pDist = Input,ClusterNo = ClusterNo,...))
  }else{
    return(HierarchicalCluster(Data = DataOrDistances,ClusterNo = ClusterNo,...))
  }
  
  tryCatch({
    out=parallel::parLapply(cl = cl,X = NumberOfTrials,fun = help_fun,FUN,DataOrDistance,ClusterNo,...)
  },error=function(e){
    print(e)
    parallel::stopCluster(cl)
  })
  parallel::stopCluster(cl)
  
  Cls_matrix=simplify2array(lapply(out, `[[`, 1),higher = FALSE)
  CompTimeVec=sapply(out, `[[`, 2)
  
  return(list(Cls_Matrix=Cls_matrix,ComputationTime=CompTimeVec))
}
