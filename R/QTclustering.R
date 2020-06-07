QTclustering=QTClustering <-function(Data,Radius,PlotIt=FALSE,...){
  # Cls=QTClustering(Data,Radius=2)
  #  
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # Radius            Maximum radius of clusters. If NULL, automatic estimation can be
  #                   done with [Thrun et al., 2016] if not otherwise set.
  # 
  # OPTIONAL
  # PlotIt            Boolean. Decision to plot or not
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of flexclust::qtclust algorithm
  #
  # Author: MT 04/2018

  if (!requireNamespace('flexclust')) {
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

  if(is.null(Radius)){ #estimate Maximum diameter of cluster, i.e. group of large distances
    if(requireNamespace("parallelDist")){
      Radius=EstimateRadiusByDistance(as.matrix(parallelDist::parallelDist(Data)))
    }
    else{
      stop('parallelDist package not loaded or installed.')
    }
  } 
  obj=flexclust::qtclust(Data,Radius,...)
  Cls=obj@cluster
  Cls[!is.finite(Cls)]=0
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
  
  if(PlotIt){
    Cls2=Cls
    Cls2[Cls2==0]=999
	ClusterPlotMDS(Data,Cls2)
  }
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=obj))
}