sotaClustering <-function(Data,ClusterNo,PlotIt=FALSE,...){
  # Cls=sotaClustering(Data,ClusterNo=2)
  # Self-organizing Tree Algorithm (SOTA)
  # liefert eine Klassenzuweisung
  # INPUT
  # Data[1:n,1:d]             Der Datensatz
  
  # ClusterNo  in soviele Cluster werden die daten eingeteilt
  
  # OUTPUT
  # Cls[1:n]                Clusterung der Daten
  # sotaObject         Object of sota Alorithm
  # Author: MT 06/2015
  #1.Editor: MT 04/18
  
  
  requireNamespace('clValid')
  res=clValid::sota(Data,maxCycles = ClusterNo-1,...)
  Cls=res$clus
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
    
  if(PlotIt){
    requireNamespace('DataVisualizations')
    DataVisualizations::plot3D(Data,res$clust)
  }
  return(list(Cls=res$clust,sotaObject=res))
}