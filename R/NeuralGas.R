NeuralGas <-function(Data,FestgesetzteClustAnz){
# Cls=NeuralGas(Data,FestgesetzteClustAnz=2)
#  
# liefert eine Klassenzuweisung
# INPUT
# Data[1:n,1:d]             Der Datensatz

# FestgesetzteClustAnz  in soviele Cluster werden die daten eingeteilt
  
# OUTPUT
# Cls[1:n]                Clusterung der Daten
#
# Author: MT 06/2015

#requireRpackage('cclust')
requireNamespace('cclust')
res=cclust::cclust(x=Data,centers=FestgesetzteClustAnz,method='neuralgas')

return(cls=res$cluster)
}