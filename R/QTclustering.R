QTClustering <-function(Data,Radius,ClusterNo,PlotIt=FALSE,...){
  # Cls=QTClustering(Data,Radius=2)
  #  
  # liefert eine Klassenzuweisung
  # INPUT
  # Data[1:n,1:d]             Der Datensatz
  # Radius  in soviele Cluster werden die daten eingeteilt
  # PlotIt

  # OrclusInitialClustersNo

  # OUTPUT
  # Cls[1:n]                Clusterung der Daten
  # QTObject
  #
  # Author: MT 04/2018

  #
  requireNamespace('flexclust')
  if(missing(Radius)){  
    warning('The Radius parameter is missing but it is required in DBscan. Trying to estimate..')
    if(is.null(ClusterNo)) stop('ClusterNo has to be set to estimate Radius.')
    Radius=sqrt(min(kmeansClustering(Data,ClusterNo=ClusterNo,method = 'LBG')$SumDistsToCentroids))
    
  } 
  obj=flexclust::qtclust(Data,Radius,...)
  Cls=obj@cluster
  
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
  
  if(PlotIt){
    requireNamespace('DataVisualizations')
    DataVisualizations::plot3D(Data,Cls)
  }
  return(list(Cls=Cls,QTObject=obj))
}