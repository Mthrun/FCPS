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

  if (!requireNamespace('flexclust',quietly = TRUE)) {
    message(
      'Subordinate clustering package (flexclust) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate clustering package (flexclust) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }

  if(is.null(Radius)){ #estimate Maximum diameter of cluster, i.e. group of large distances
    if(requireNamespace("parallelDist",quietly = TRUE)){
      Radius=EstimateRadiusByDistance(as.matrix(parallelDist::parallelDist(Data)))
    }
    else{
      warning('parallelDist package not loaded or installed, using dist()')
      Radius=EstimateRadiusByDistance(as.matrix(dist(Data)))
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