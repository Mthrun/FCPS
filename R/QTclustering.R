QTClustering <-function(Data,Radius=TRUE,PlotIt=FALSE,...){
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
    if(Radius==TRUE){
      Radius=AdaptGauss::ParetoRadius(Data)
    }
    if(Radius==FALSE){
      requireNamespace('ABCanalysis')
      x=as.matrix(dist(Data))
      x=x[lower.tri(x, diag = FALSE)]
      par=quantile(x,c(0.2013)) #geschaetzter paretorRadius
      xx=ABCanalysis::ABCRemoveSmallYields(x,0.5)
      x=xx$SubstantialData
      res=suppressWarnings(ABCanalysis::ABCanalysis(x))
      Radius=min(x[res$Aind])/max(x[res$Cind])
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