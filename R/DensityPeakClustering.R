DensityPeakClustering=function(DataOrDistances,Knn=10,Rho,Delta,method="euclidean",PlotIt=FALSE,...){
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

  out=densityClust::densityClust(Distances,k=Knn,...)

  if(missing(Rho)|missing(Delta)){
    plot(out)
	return('Please set Paramaters Rho and Delta')
  }else{
    out=densityClust::findClusters(out,rho=Rho,delta=Delta)
  }
  Cls=out$clusters

  if(sum(is.na(Cls))==length(Cls)){
  	Cls=rep(1,nrow(Distances))
  	warning('NoCluster could be found.')
  }
  if(PlotIt){
    requireNamespace('DataVisualizations')
    if (!isSymmetric(DataOrDistances)) {
      DataVisualizations::Plot3D(DataOrDistances,Cls)
    }else{
      requireNamespace('ProjectionBasedClustering')
      
      DataVisualizations::Plot3D(DataOrDistances,ProjectionBasedClustering::MDS(DataOrDistances,OutputDimension = 3)$ProjectedPoints)
    }
  }
  return(list(Cls=Cls,DPobject=out))
} 