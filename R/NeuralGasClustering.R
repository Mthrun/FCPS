NeuralGasClustering <-function(Data,ClusterNo,PlotIt=FALSE,...){
# Cls=NeuralGas(Data,ClusterNo)
#  
# liefert eine Klassenzuweisung
# INPUT
# Data[1:n,1:d]             Der Datensatz

# ClusterNo  in soviele Cluster werden die daten eingeteilt
  
# OUTPUT
# Cls[1:n]                Clusterung der Daten
# NeuralGasObject         Object of NeuralGas Alorithm
# Author: MT 06/2015
#1.Editor: MT 04/18

  
  if (!requireNamespace('cclust')) {
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
  
  

res=cclust::cclust(x=Data,centers=ClusterNo,method='neuralgas',...)
Cls=res$cluster
if(PlotIt){
  ClusterPlotMDS(Data,Cls)
}
Cls=ClusterRename(Cls,Data)
return(list(Cls=Cls,Object=res))
}