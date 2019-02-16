pdfClustering <-function(Data,PlotIt=FALSE,...){
  # Cls <- pdfClustering(Data,ClusterNo);
  # Clustering via nonparametric density estimation
  # INPUT
  # Data[1:n]               der Datensatz in Zeilenvektoren 
  #
  # OUTPUT List V with
  # Cls[1:n]                k-means Clusterung der Daten
  # MT 2019



  requireNamespace('pdfCluster')
  out=pdfCluster::pdfCluster(Data,...)
  
  Cls=as.vector(out@clusters)
  
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
  
  if(PlotIt){
    requireNamespace('DataVisualizations')
    DataVisualizations::Plot3D(Data,Cls)
  }
  return(list(Cls=Cls,MClustObject=out))
}