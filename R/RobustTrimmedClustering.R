RobustTrimmedClustering=function(Data,ClusterNo,Alpha,PlotIt=FALSE,...){
  # Cls=RobustTrimmedClustering(Data,ClusterNo)
  # liefert eine Klassenzuweisung
  # INPUT
  # Data[1:n,1:d]             Der Datensatz
  
  # ClusterNo  in soviele Cluster werden die daten eingeteilt
  
  # OUTPUT
  # Cls[1:n]                Clusterung der Daten
  # TClustObject         Object of sota Alorithm
  # Author: MT 09/2019  
  
  #
  
  if (!requireNamespace('tclust')) {
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
  
  
  
  
  res=tclust::tclust(x=Data,k = ClusterNo,alpha = Alpha,...)
  Cls=res$cluster	
  
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=res))
}