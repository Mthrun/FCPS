DivisiveAnalysisClustering <-function(Data,ClusterNo,PlotIt=FALSE,...){
  # Cls=DivisiveAnalysisClustering(Data,ClusterNo=2)
  # DivisiveAnalysisClustering (diana)
  # liefert eine Klassenzuweisung
  # INPUT
  # Data[1:n,1:d]             Der Datensatz
  # ClusterNo  in soviele Cluster werden die daten eingeteilt
  
  # OUTPUT
  # Cls[1:n]                Clusterung der Daten
  # dianaObject         Object of sota Alorithm
  # Author: MT 04/2018
  
  
  requireNamespace('cluster')
  res=cluster::diana(x=Data,...)
  if(length(ClusterNo)!=1){
    stop('ClusterNo has to be an numerical number not a vector of length higher than 1 or another object.')
  }
  if(ClusterNo>0){
    Cls=cutree(as.hclust(res), k = ClusterNo)
    
    if(PlotIt){
      requireNamespace('DataVisualizations')
      DataVisualizations::plot3D(Data,Cls)
      #plot(res)
    }
  }
  if(ClusterNo<=0){
    Cls=NULL
    plot(res)
    if(ClusterNo<0){
      warning(('ClusterNo cannot be a negativ number'))
    }
  }
  return(list(Cls=Cls,dianaObject=res))
}