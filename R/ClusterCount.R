ClusterCount <- function(Cls,Ordered=FALSE) {
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
  Cls[!is.finite(Cls)]=9999
  
  countPerCluster=table(Cls)
  u= unique(Cls,fromLast = FALSE)
 
  uniqueClusters = as.numeric(names(countPerCluster)) #order ist not as is!
  
  ind=match(u,table = uniqueClusters)
  
  numberOfClusters = length(uniqueClusters)
  
  ClusterPercentages = as.numeric(prop.table(countPerCluster)*100)
  
  Overview=cbind(
  uniqueClusters[ind],
  as.numeric(countPerCluster)[ind],
  ClusterPercentages[ind]
  )
  if(isTRUE(Ordered)){
    ind=order(Overview[,1],decreasing = FALSE,na.last = T)
    Overview=Overview[ind,]
  }

  return(
    list(
      UniqueClusters = Overview[,1],
      CountPerCluster = Overview[,2],
      NumberOfClusters = numberOfClusters,
      ClusterPercentages = Overview[,3]
    )
  )
}
