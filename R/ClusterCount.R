ClusterCount <- function(Cls) {
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
  
  uniqueClusters <- as.numeric(names(countPerCluster))
  numberOfClusters <- length(uniqueClusters)
  
  ClusterPercentages = as.numeric(prop.table(countPerCluster)*100)
  return(
    list(
      UniqueClusters = uniqueClusters,
      CountPerCluster = as.numeric(countPerCluster),
      NumberOfClusters = numberOfClusters,
      ClusterPercentages = ClusterPercentages
    )
  )
}
