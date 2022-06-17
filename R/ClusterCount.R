ClusterCount <- function(Cls,Ordered=TRUE,NonFinite=9999) {
  # Calculates statistics for clustering
  # C <-ClusterCount(Cls)
  # UniqueClusters <-C$UniqueClusters
  # CountPerCluster <-C$CountPerCluster
  # NrOfClusters   <-C$NumberOfClusters
  # ClusterPercentages <-C$ClusterPercentages
  #
  # INPUT
  # Cls[d]                          numeric vector such as Cls(i) == ClusterNumber of Data[i,] of point i
  #
  # OUTPUT list with:
  # UniqueClusters[1:NrOfClusters]     NrOfClusters unique Clusters in Cls
  # CountPerCluster(NrOfClusters,n)    CountPerCluster(i) is the Count of the data points in UniqueClusters(i)
  # NumberOfClusters                   Number of Clusters
  # ClusterPercentages                 Percentages of the Clusters
  #
  # Author MT
  
  if(!is.vector(Cls)){
    warning('ClusterCount: Cls is not a vector. Calling as.numeric(as.character(Cls))')
    Cls=as.numeric(as.character(Cls))
  }
  Cls[!is.finite(Cls)]=NonFinite

  if(isFALSE(Ordered)){
    countPerCluster=table(Cls)
    u= unique(Cls,fromLast = FALSE)
    uniqueClusters = as.numeric(names(countPerCluster)) #order ist not as is!
    ind=match(u,table = uniqueClusters)
    uniqueClusters=uniqueClusters[ind]
    countPerCluster=as.numeric(countPerCluster)[ind]
  }else{
    #radix: fasted sort of numeric
    #rle, run length encoding, counts number of consecutive values
    V=rle(sort(Cls,method="radix"))
    countPerCluster=V$lengths
    uniqueClusters=V$values
  }
  
  numberOfClusters = length(uniqueClusters)
  ClusterPercentages = as.numeric(prop.table(countPerCluster)*100)
  
  # Overview=cbind(
  #   uniqueClusters[ind],
  #   as.numeric(countPerCluster)[ind],
  #   ClusterPercentages[ind]
  # )
  # if(isTRUE(Ordered)){
  #   ind=order(Overview[,1],decreasing = FALSE,na.last = T)
  #   Overview=Overview[ind,,drop=FALSE]
  # }
  names(countPerCluster)=uniqueClusters
  return(
    list(
      UniqueClusters = uniqueClusters,
      CountPerCluster = countPerCluster,
      NumberOfClusters = numberOfClusters,
      ClusterPercentages = ClusterPercentages
    )
  )
}
