ADPclustering=function(Data,ClusterNo=NULL,PlotIt=FALSE,...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Number of clusters to search for
  #
  # OPTIONAL
  # PlotIt            Boolean. Decision to plot or not
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of adpclust
  #
  # Author: MT, 04/2018
  if (!requireNamespace('ADPclust')) {
    message(
      'Subordinate clustering package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate clustering package is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  if(is.null(ClusterNo))
	adp=ADPclust::adpclust(Data,...)
  else
  adp=ADPclust::adpclust(Data,nclust=ClusterNo,...)
  
  Cls=as.numeric(adp$clusters)
  Cls=ClusterRename(Cls,Data)
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  return(list(Cls=Cls,Object=adp))
}

#other package, were params have to be chosen
# DensityPeakClustering=function(DataOrDistances,Knn=10,rho=2, delta=2, method="euclidean",PlotIt=TRUE,...){
#   #Rodriguez, A., & Laio, A.: Clustering by fast search and find of density peaks. Science, 344(6191), 1492-1496. doi:10.1126/science.1242072, 2014.
#   requireNamespace('densityClust')
#   if(!is.matrix(DataOrDistances)){
#     warning('DataOrDistances is not a matrix. Calling as.matrix()')
#     DataOrDistances=as.matrix(DataOrDistances)
#   }
#   if(!mode(DataOrDistances)=='numeric'){
#     warning('Data is not a numeric matrix. Calling mode(DataOrDistances)="numeric"')
#     mode(DataOrDistances)='numeric'
#   }
#   AnzData = nrow(DataOrDistances)
#   
#   if (!isSymmetric(DataOrDistances)) {
#     requireNamespace('parallelDist')
#     
#     Distances=as.matrix(parallelDist::parDist(DataOrDistances,method=method))
#   }
#   
#   out=densityClust::densityClust(Distances,...)
#   
#   if(PlotIt){
#     requireNamespace('DataVisualizations')
#     if (!isSymmetric(DataOrDistances)) {
#       DataVisualizations::Plot3D(DataOrDistances,Cls,k=Knn)
#     }else{
#       requireNamespace('ProjectionBasedClustering')
#       
#       DataVisualizations::Plot3D(DataOrDistances,ProjectionBasedClustering::MDS(DataOrDistances,OutputDimension = 3)$ProjectedPoints)
#     }
#   }
#   return(list(Cls=NULL,DPobject=out))
# } 