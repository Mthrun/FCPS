SharedNearestNeighborClustering <-function(Data,Knn=7,Radius,minPts,PlotIt=FALSE,UpperLimitRadius,...){
  # Cls=SharedNearestNeighborClustering(FCPS$Hepta$Data,sqrt(min(res$withinss)))
  # DBscan nach [Ester et al., 1996]
  # 
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # Radius            eps,  radius of R-ball [Ester et al., 1996, p. 227],size of the epsilon neighborhood.
  # 
  # OPTIONAL
  # Knn               Number of neighbors to consider to calculate the shared nearest neighbors.
  # Radius            eps [Ester et al., 1996, p. 227] neighborhood in the R-ball graph/unit disk graph),
  #                   size of the epsilon neighborhood. If NULL, automatic estimation is done using
  #                   insights of [Ultsch, 2005].
  # minPts            In principle minimum number of points in the unit disk, if the unit disk is within the cluster (core) [Ester et al., 1996, p. 228].
  #                   number of minimum points in the eps region (for core points). Default is 5 points.
  # PlotIt            Boolean. Decision to plot or not
  # UpperLimitRadius  Limit for radius search, experimental
  # 
  # OUTPUT
  # Cls[1:n]    Clustering of data. Points which cannot be assigned to a cluster will be reported as members of the noise cluster with NaN.
  # Object      Object defined by clustering algorithm as the other output of this algorithm
  # 
  # Author: MT 2019
  
  if (!requireNamespace('dbscan')) {
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
  
  if(is.null(nrow(Data))){# then we get a vector
    return(cls <- rep(1,length(Data)))
  }
  
  if(is.null(Radius)){  
    if(requireNamespace("DataVisualizations")){
      warning('The Radius (eps) parameter is missing but it is required in DBscan. Trying to estimate..')
      Radius=0.5*DataVisualizations::ParetoRadius(Data)
    }
    else{
      stop('DataVisualizations package not loaded or installed.')
    }
  } 
  if(is.null(minPts)){
    minPts=min(round(0.0005*nrow(Data),2),20)## A point needs at least 16 (minPts) links in the sNN graph to be a core point.
    warning('The minPts parameter is missing but it is required in DBscan. Trying to estimate..')
  }   
  if(missing(UpperLimitRadius))
    UpperLimitRadius=2*Radius

  liste=dbscan::sNNclust(x = Data,k=Knn,eps=Radius,minPts=minPts,...)
  Cls=liste$cluster

  ind=which(Cls==0)

  Cls[!is.finite(Cls)]=0
  # Noise points have cluster id 0
  if(Radius<UpperLimitRadius&sum(Cls==0)>round(0.025*nrow(Data))){
    out=suppressWarnings(SharedNearestNeighborClustering(Data,Knn=Knn,Radius=Radius*1.01,minPts=minPts,PlotIt=PlotIt,UpperLimitRadius=UpperLimitRadius,...))
    Cls=out$Cls
    liste=out$SNNobject
  }
  
  if(!is.null(rownames(Data))){
    names(Cls)=rownames(Data)
  }
  
  if(PlotIt){
    Cls2=Cls
    Cls2[Cls2==0]=999
    ClusterPlotMDS(Data,Cls2)
  }
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=liste))
}
