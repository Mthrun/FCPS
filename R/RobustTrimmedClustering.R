RobustTrimmedClustering=function(Data,ClusterNo,PlotIt=FALSE,...){
  # Cls=RobustTrimmedClustering(Data,ClusterNo)
  # liefert eine Klassenzuweisung
  # INPUT
  # Data[1:n,1:d]             Der Datensatz
  
  # ClusterNo  in soviele Cluster werden die daten eingeteilt
  
  # OUTPUT
  # Cls[1:n]                Clusterung der Daten
  # TClustObject         Object of sota Alorithm
  # Author: MT 09/2019  
  
  
  requireNamespace('tclust')
  res=tclust::tclust(x=Data,k = ClusterNo,...)
  Cls=res$cluster	
  
  if(PlotIt){
    requireNamespace('DataVisualizations')
    DataVisualizations::Plot3D(Data,Cls)
  }
  return(list(Cls=Cls,TClustObject=res))
}