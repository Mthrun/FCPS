ClusterApply <- function(DataOrDistances, FUN, Cls,Simple=FALSE,...){
  #
  # Applies a given function to each dimension for each cluster in Cls for all observations in the data.
  #
  # INPUT
  # DataOrDistances(n,d)    n cases, d variables
  # FUN                     Function to be applied
  # Cls(n)                  Cls(i) == ClusterNumber of Data(i,:)
  # 
  # OUTPUT
  # UniqueClasses    The  AnzCluster unique clusters in Cls
  # FUNPerCluster    FUNPerCluster[i] is the result of FUN for the data points in Cluster UniqueClusters[i]
  #
  # 
  if(!is.matrix(DataOrDistances)){
    warning('ClusterApply: DataOrDistances is not a matrix. Calling as.matrix')
    DataOrDistances=as.matrix(DataOrDistances)
  }
  if(mode(DataOrDistances)!="numeric"){
    warning('ClusterApply: DataOrDistances is not numeric, setting mode to numeric.')
    mode(DataOrDistances)="numeric"
  }
  if (IsDissimilarity(DataOrDistances)) {
    Data=internalMDSestimate(DataOrDistances)
  }else{
    Data=DataOrDistances
  }	
  if(missing(Cls)){
    Cls=rep(1,nrow(Data))
  }
  if(is.list(Cls)){
    warning('ClusterApply: Cls is a list. Calling unlist')
    Cls=unlist(Cls)
  }
  if(!is.vector(Cls)){
    warning('ClusterApply: Cls is not a vector. Calling as.vector')
    Cls=as.vector(Cls)
  }
  Names=colnames(Data)

  
  #Option 2
  if(isFALSE(Simple)){
  Liste=split(x = as.data.frame(Data),f = Cls)
  uniqueClusters=names(Liste)
  PerClusterV=lapply(
    Liste,
    function(x) apply(x, FUN = FUN, MARGIN = 2, ...)
  )
  resultPerCluster=do.call(rbind,PerClusterV)
  
  if(!is.null(Names)){
    try({
      colnames(resultPerCluster)=Names
    })
  }
   try({
     
     if(length(uniqueClusters)==nrow(resultPerCluster))
        rownames(resultPerCluster)=uniqueClusters
     else
       rownames(resultPerCluster)=NULL
    })

  fun_name <- paste(deparse(substitute(FUN)), collapse = "")
  result_name <- if (nzchar(fun_name)) {
    paste0(fun_name, "PerCluster")
  } else {
    "ResultPerCluster"
  }

  V <- list(
    UniqueClusters = uniqueClusters,
    ResultPerCluster = resultPerCluster
  )
  names(V)[2L] <- result_name
  }else{
    V=apply(Data,2,function(X,...) tapply(X, Cls, FUN = FUN,...))
  }
  return(V)
}