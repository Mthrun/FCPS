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
  Cls[!is.finite(Cls)]=0
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
  
  if(PlotIt){
    Cls2=Cls
    Cls2[Cls2==0]=999
    requireNamespace('DataVisualizations')
    DataVisualizations::Plot3D(Data,Cls2)
  }
  return(list(Cls=Cls,QTObject=obj))
}