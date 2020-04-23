ClusterApply <- function(Data, Cls, Func, ...){
  # Applies a given function to each dimension for each cluster in Cls over the data.
   # INPUT
  # Data(d,n)         d cases,  n variables
  # Cls(d)            Cls(i) == ClusterNumber of Data(i,:)
  # Func              Function to be applied
  # OUTPUT
  # UniqueClasses[AnzCluster]             the  AnzCluster unique clusters in Cls
  # FuncPerCluster[AnzCluster,Columns]    FuncPerCluster[i] is the result of func for the data points in Cluster UniqueClusters[i]
  if(is.list(Cls)){
    warning('ClusterApply: Cls is a list. Calling unlist')
    Cls=unlist(Cls)
  }
  if(!is.vector(Cls)){
    warning('ClusterApply: Cls is not a vector. Calling as.vector')
    Cls=as.vector(Cls)
  }
  if(!is.matrix(Data)){
    warning('ClusterApply: Data is not a matrix Calling as.matrix')
    Data=as.matrix(Data)
  }
  if(mode(Data)!="numeric"){
    warning('ClusterApply: Data is not a numeric, setting mode to numeric.')
    mode(Data)="numeric"
  }
  Names=colnames(Data)
  uniqueClusters <- sort(na.last = T, unique(Cls))
  numberOfClusters <- length(uniqueClusters)
  resultPerCluster <- matrix(0, numberOfClusters, ncol(Data))
  
  for (i in 1:numberOfClusters) {
    inClusterInd <- which(Cls == uniqueClusters[i])
    x = Data[inClusterInd, ]
    if(is.vector(x)) { # Wenns nur ein Datensatz, also ein Vektor, ist, macht R mist. Also konvertieren.
      margin = 1
      x = as.matrix(x)
    } else {
      margin = 2
    }
    resultPerCluster[i, ]  <-
      apply(
        X = x,
        FUN = Func,
        MARGIN = margin,
        ...
      )
  }
  if(!is.null(names)){
    try({
      colnames(resultPerCluster)=Names
    })
  }

  V=list(UniqueClusters = uniqueClusters, ResultPerCluster = resultPerCluster)
  
  try({
    string=as.character(substitute(mean))
    names(V)=c('UniqueClusters',paste0(string,'PerCluster'))
  })
  return(V)
}