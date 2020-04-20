LargeApplicationClustering <-function(Data,ClusterNo,PlotIt=FALSE,Standardization=TRUE,Samples=50,Random=TRUE,...){
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
  if(Standardization==1) Standardization=TRUE
  if(Standardization==0) Standardization=FALSE
  
  requireNamespace('cluster')
  res=cluster::clara(x=Data,k = ClusterNo,samples=Samples,rngR=Random,stand=Standardization,...)
  Cls=res$clustering

  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  	Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=res))
}