DensityPeakClustering=function(DataOrDistances,Rho,Delta,Dc,Knn=7,method="euclidean",PlotIt=FALSE,...){
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
  if(missing(Dc))
    DensityPeaks=densityClust::densityClust(Distances,k=Knn,...)
  else
    DensityPeaks=densityClust::densityClust(Distances,dc=Dc,k=Knn,...)
  
  if(missing(Rho)|missing(Delta)){
    requireNamespace('plotly')
    print('Please set Paramaters Rho and Delta')
   
    p <- plotly::plot_ly( x = ~DensityPeaks$rho, y = ~DensityPeaks$delta,type = "scatter",mode="markers")
    p=plotly::layout(p,title = "Decision Graph",xaxis=list(exponentformat = "E",  title = "Local Density Rho"),yaxis=list(exponentformat = "E",  title = "Minimum Distance Delta"),showlegend = FALSE)
	return(p)
  }else{
    DensityPeaks=densityClust::findClusters(DensityPeaks,rho=Rho,delta=Delta)
  }
  Cls=DensityPeaks$clusters

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
  return(list(Cls=Cls,DPobject=DensityPeaks))
} 