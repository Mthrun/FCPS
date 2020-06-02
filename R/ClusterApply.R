ClusterApply <- function(DataOrDistances, FUN, Cls, ...){
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
  if (isSymmetric(unname(DataOrDistances))) {
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
  uniqueClusters <- sort(na.last = T, unique(Cls))
  numberOfClusters <- length(uniqueClusters)
  #resultPerCluster <- matrix(0, numberOfClusters, ncol(Data))

  for (i in 1:numberOfClusters) {
    inClusterInd <- which(Cls == uniqueClusters[i])
    x = Data[inClusterInd, ]
    if(is.vector(x)) { # Creates problems if there is only one data point
      margin = 1
      x = as.matrix(x)
    } else {
      margin = 2
    }
     y<-
      apply(
        X = x,
        FUN = FUN,
        MARGIN = margin,
        ...
      )
    if(i==1)
      resultPerCluster=matrix(y,ncol = ncol(Data))
    else
      resultPerCluster=rbind(resultPerCluster,y)
  }
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

  V=list(UniqueClusters = uniqueClusters, ResultPerCluster = resultPerCluster)
  
  tryCatch({
    string=as.character(substitute(FUN))
    names(V)=c('UniqueClusters',paste0(string,'PerCluster'))
  },error=function(e){
    message('ClusterApply: FUN could not be extracted because:')
    message(e)
  })
  return(V)
}