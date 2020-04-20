MSTclustering=function(DataOrDistances,method="euclidean",PlotIt=FALSE,...){
  requireNamespace('mstknnclust')
  if(!is.matrix(DataOrDistances)){
    warning('DataOrDistances is not a matrix. Calling as.matrix()')
    DataOrDistances=as.matrix(DataOrDistances)
  }
  if(!mode(DataOrDistances)=='numeric'){
    warning('Data is not a numeric matrix. Calling mode(DataOrDistances)="numeric"')
    mode(DataOrDistances)='numeric'
  }
  AnzData = nrow(DataOrDistances)
  
  if (!isSymmetric(unname(DataOrDistances))) {
    requireNamespace('parallelDist')
    
    Distances=as.matrix(parallelDist::parDist(DataOrDistances,method=method))
  }else{
    Distances=DataOrDistances
  }
  results <- mstknnclust::mst.knn(distance.matrix = Distances,...)
  Cls=results$cluster
  if(isTRUE(PlotIt)){
	ClusterPlotMDS(DataOrDistances,Cls)
  }
  Cls=ClusterRename(Cls,DataOrDistances)
  return(list(Cls=Cls,Object=results))
}