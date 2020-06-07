DensityPeakClustering=function(DataOrDistances,Rho,Delta,Dc,Knn=7,method="euclidean",PlotIt=FALSE,Data,...){
  #
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # Rho               Local density of a point, see [Rodriguez/Laio, 2014] for explanation 
  # Delta             Minimum distance between a point and any other point, see [Rodriguez/Laio, 2014] for explanation
  # Dc                Optional, cutoff distance, will either be estimated by [Pedersen et al., 2017] or
  #                   [Wang et al, 2015] (see example below)
  #
  # OPTIONAL
  # Knn               Optional k nearest neighbors
  # method            Optional distance method of data, default is euclid
  # PlotIt            Boolean. Decision to plot or not
  #
  # OUTPUT
  # Cls[1:n]        Clustering of data
  # Object          Object of adpclust algorithm
  #
  # Author: MT
  # Rodriguez, A., & Laio, A.: Clustering by fast search and find of density peaks. Science, 344(6191), 1492-1496. doi:10.1126/science.1242072, 2014.

  
  if (!requireNamespace('densityClust')) {
    message(
      'Subordinate clustering package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(DataOrDistances)),
        Object = "Subordinate clustering package is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  if(missing(DataOrDistances)){
    DataOrDistances=Data
  }
  
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
  if(missing(Dc))
    DensityPeaks=densityClust::densityClust(Distances,k=Knn,...)
  else
    DensityPeaks=densityClust::densityClust(Distances,dc=Dc,k=Knn,...)
  
  if(missing(Rho)|missing(Delta)){
    requireNamespace('plotly')
    print('Please set parameters Rho and Delta')
   
    p <- plotly::plot_ly( x = ~DensityPeaks$rho, y = ~DensityPeaks$delta,type = "scatter",mode="markers")
    p=plotly::layout(p,title = "Decision graph",xaxis=list(exponentformat = "E",  title = "Local Density Rho"),yaxis=list(exponentformat = "E",  title = "Minimum Distance Delta"),showlegend = FALSE)
	return(p)
  }else{
    DensityPeaks=densityClust::findClusters(DensityPeaks,rho=Rho,delta=Delta)
  }
  Cls=DensityPeaks$clusters

  if(sum(is.na(Cls))==length(Cls)){
  	Cls=rep(1,nrow(Distances))
  	warning('No cluster could be found.')
  }
  if(PlotIt){
	ClusterPlotMDS(DataOrDistances,Cls)
  }
  Cls=ClusterRename(Cls,DataOrDistances)
  return(list(Cls=Cls,Object=DensityPeaks))
} 