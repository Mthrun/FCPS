DensityPeakClustering=function(DataOrDistances,Knn=10,method="euclidean",PlotIt=TRUE,...){
  #Rodriguez, A., & Laio, A.: Clustering by fast search and find of density peaks. Science, 344(6191), 1492-1496. doi:10.1126/science.1242072, 2014.
  requireNamespace('densityClust')
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
  }

  out=densityClust::densityClust(Distances,...)
  
  if(PlotIt){
    requireNamespace('DataVisualizations')
    if (!isSymmetric(DataOrDistances)) {
      DataVisualizations::plot3D(DataOrDistances,Cls)
    }else{
      requireNamespace('ProjectionBasedClustering')
      
      DataVisualizations::plot3D(DataOrDistances,ProjectionBasedClustering::MDS(DataOrDistances,OutputDimension = 3)$ProjectedPoints)
    }
  }
  return(list(Cls=NULL,DPobject=out))
} 