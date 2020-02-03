QTclustering=QTClustering <-function(Data,Radius,PlotIt=FALSE,...){
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

    if(missing(Radius)){ #estimate Maximum diameter of cluster, i.e. group of large distances
      requireNamespace('parallelDist')
      Radius=EstimateRadiusByDistance(as.matrix(parallelDist::parallelDist(Data)))
    } 
  obj=flexclust::qtclust(Data,Radius,...)
  Cls=obj@cluster
  Cls[!is.finite(Cls)]=0
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
  
  if(PlotIt){
    Cls2=Cls
    Cls2[Cls2==0]=999
    requireNamespace('DataVisualizations')
    DataVisualizations::Plot3D(Data,Cls2)
  }
  return(list(Cls=Cls,Object=obj))
}