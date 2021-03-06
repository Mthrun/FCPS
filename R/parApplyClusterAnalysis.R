parApplyClusterAnalysis=function(DataOrDistances,FUN,NumberOfTrials=1:100,ClusterNo=NULL,WorkersOrNo,SocketType="PSOCK",SetSeed=TRUE,...){
  #example
  # data(Hepta)
  # Distance=as.matrix(parallelDist::parallelDist(Hepta$Data))
  # out=parApplyClusterAnalysis(DataOrDistances = Distance,FUN=APclustering,ClusterNo = 7)
  if(!is.list(DataOrDistances)){
    if(!is.null(WorkersOrNo)){
      if(!requireNamespace('parallel')){
        message('package parrellels not availalbe. Using simple lapply function.')
        WorkersOrNo=NULL
      }
    }
 
  ShutDownAfter=TRUE
  if(missing(WorkersOrNo)){
    WorkersOrNo <- parallel::detectCores() - 1
  }
  if(is.na(ClusterNo)) ClusterNo=NULL
  
  if(any(class(WorkersOrNo)=="cluster")){
    message("Use clusters...")
      cl=WorkersOrNo
      ShutDownAfter=FALSE
    }else{
      if(!is.null(WorkersOrNo)){
        message("Make clusters...")
        cl=parallel::makeCluster(WorkersOrNo,type = SocketType)
      }
  }
  

  message("Compute Benchmarking of Clustering Method")
  #parallel::clusterExport(cl = cl,varlist = c(deparse(substitute(FUN)),'cluster_analysis_fun'))
  
  string=names(formals(FUN))
  if(!is.null(WorkersOrNo)){
    tryCatch({
      if(string[1]=="Data") {
        Data=DataOrDistances
        if(!isSymmetric(unname(Data)))
          out=parallel::parLapply(cl = cl,X = NumberOfTrials,fun = cluster_analysis_fun,FUN,Data,ClusterNo,SetSeed,...)
        else
          stop(paste('Clustering algorithm',deparse(substitute(FUN)),'is unable to use a distance matrix. Please provide a data matrix.'))
      }else{
          out=parallel::parLapply(cl = cl,X = NumberOfTrials,fun = cluster_analysis_fun,FUN,DataOrDistances,ClusterNo,SetSeed,...)
      }
  
    },error=function(e){
      print(e)
      #if(ShutDownAfter)
        parallel::stopCluster(cl)
    })
    
    if(ShutDownAfter)
      try(parallel::stopCluster(cl))
  }else{
    if(string[1]=="Data") {
      Data=DataOrDistances
      if(!isSymmetric(unname(Data)))
        out=lapply(X = NumberOfTrials,FUN = cluster_analysis_fun,FUN,Data,ClusterNo,SetSeed,...)
      else
        stop(paste('Clustering algorithm',deparse(substitute(FUN)),'is unable to use a distance matrix. Please provide a data matrix.'))
    }else{
      out=lapply(X = NumberOfTrials,FUN = cluster_analysis_fun,FUN,DataOrDistances,ClusterNo,SetSeed,...)
    }
  }

  
  Cls_matrix=simplify2array(lapply(out, `[[`, 1),higher = FALSE)
  CompTimeVec=sapply(out, `[[`, 2)
  Seeds=sapply(out, `[[`, 3)
  
  return(list(Cls_Matrix=Cls_matrix,ComputationTime=CompTimeVec,Seeds=Seeds))
  }else{#data is list
    if(missing(WorkersOrNo)){
      WorkersOrNo <- parallel::detectCores() - 1
    }
    
    if(any(class(WorkersOrNo)=="cluster")){
      message("Use clusters...")
      cl=WorkersOrNo
      ShutDownAfter=FALSE
    }else{
      if(!is.null(WorkersOrNo)){
        message("Make clusters...")
        cl=parallel::makeCluster(WorkersOrNo,type = SocketType)
      }
    }
    
    Datanames=names(DataOrDistances)
    Benchmarking=list()
    N=length(DataOrDistances)
    if(N!=length(ClusterNo)){
      ClusterNo=c(ClusterNo,rep(ClusterNo[1],N-length(ClusterNo)))
      warning('parApplyClusterAnalysis: ClusterNo is not of length of list of DataOrDistances. Extending to equal lengths with ClusterNo[1] = ',ClusterNo[1],'. Benchmarking may not work correctly.')
    }
    for(i in 1:N){
      DataOrDistancesCur=DataOrDistances[[i]]
      ClusterNoCur=ClusterNo[i]
      message(paste('Computing Dataset',Datanames[i],'out of',N))
      if(!is.null(WorkersOrNo))
        Benchmarking[[i]]=parApplyClusterAnalysis(DataOrDistances=DataOrDistancesCur,FUN,NumberOfTrials=NumberOfTrials,ClusterNo=ClusterNoCur,WorkersOrNo=cl,SocketType=NULL,SetSeed=SetSeed,...)
      else
        Benchmarking[[i]]=parApplyClusterAnalysis(DataOrDistances=DataOrDistancesCur,FUN,NumberOfTrials=NumberOfTrials,ClusterNo=ClusterNoCur,WorkersOrNo=NULL,SocketType=NULL,SetSeed=SetSeed,...)
    }
    names(Benchmarking)=Datanames
    return(Benchmarking)
  }
}
