pdfClustering <-function(Data,PlotIt=FALSE,...){
  # Cls <- pdfClustering(Data,ClusterNo);
  # Clustering via nonparametric density estimation
  # INPUT
  # Data[1:n]               der Datensatz in Zeilenvektoren 
  #
  # OUTPUT List V with
  # Cls[1:n]                k-means Clusterung der Daten
  # MT 2019

  if (!requireNamespace('pdfCluster')) {
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


  out=pdfCluster::pdfCluster(Data,...)
  
  Cls=as.vector(out@clusters)
  
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
  
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=out))
}