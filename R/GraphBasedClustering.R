GraphBasedClustering=function(DataOrDistances,method="euclidean",PlotIt=FALSE,...){
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
  
  if (!isSymmetric(DataOrDistances)) {
    requireNamespace('parallelDist')
    
    Distances=as.matrix(parallelDist::parDist(DataOrDistances,method=method))
  }else{
    Distances=DataOrDistances
  }
  results <- mstknnclust::mst.knn(distance.matrix = Distances,...)
  Cls=results$cluster
  if(isTRUE(PlotIt)){
    requireNamespace('DataVisualizations')
    if (!isSymmetric(DataOrDistances)) {
      print(DataVisualizations::Plot3D(DataOrDistances,Cls))
      
    }else{
      requireNamespace('ProjectionBasedClustering')
      
      DataVisualizations::Plot3D(DataOrDistances,ProjectionBasedClustering::MDS(DataOrDistances,OutputDimension = 3)$ProjectedPoints)
    }
  }
  return(list(Cls=Cls,mstknnclustObject=results))
}