pdfClustering <-function(Data,PlotIt=FALSE,...){
  # Cls <- pdfClustering(Data,ClusterNo);
  # Clustering via nonparametric density estimation
  # 
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  #
  # OPTIONAL
  # PlotIt            Boolean. Decision to plot or not
  # 
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of pdfCluster::pdfCluster algorithm
  # 
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
  
  if(!is.null(rownames(Data))){
    names(Cls)=rownames(Data)
  }
  
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=out))
}