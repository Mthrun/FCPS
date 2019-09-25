HierarchicalClustering=function(DataOrDistances,ClusterNo,...){
  if (isSymmetric(DataOrDistances)) {
    if(!inherits(DataOrDistances,'dist'))
      Input=as.dist(DataOrDistances)
    else
      Input=DataOrDistances
    
    return(HierarchicalClusterDists(pDist = Input,ClusterNo = ClusterNo,...))
  }else{
    return(HierarchicalCluster(Data = DataOrDistances,ClusterNo = ClusterNo,...))
  }
  
}