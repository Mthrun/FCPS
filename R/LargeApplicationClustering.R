LargeApplicationClustering <-function(Data,ClusterNo,PlotIt=FALSE,...){
  # Cls=LargeApplicationClustering(Data,ClusterNo=2)
  # Clustering Large Applications  (clara)
  # liefert eine Klassenzuweisung
  # INPUT
  # Data[1:n,1:d]             Der Datensatz
  
  # ClusterNo  in soviele Cluster werden die daten eingeteilt
  
  # OUTPUT
  # Cls[1:n]                Clusterung der Daten
  # claraObject         Object of sota Alorithm
  # Author: MT 04/2018
  
  
  requireNamespace('cluster')
  res=cluster::clara(x=Data,k = ClusterNo,...)
  Cls=res$clustering

  if(PlotIt){
    requireNamespace('DataVisualizations')
    DataVisualizations::plot3D(Data,Cls)
  }
  return(list(Cls=Cls,claraObject=res))
}