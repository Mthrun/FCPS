SharedNearestNeighborClustering <-function(Data,Knn=7,Radius,minPts,PlotIt=FALSE,UpperLimitRadius,...){
  #  Cls=SharedNearestNeighborClustering(FCPS$Hepta$Data,sqrt(min(res$withinss)))
  # DBscan nach [Ester et al., 1996]
  # INPUT
  # Data[1:n,1:d]          der Datensatz 
  # Radius                 eps,  Radius der R-kugel [Ester et al., 1996, p. 227],size of the epsilon neighborhood.
  # OPTIONAL
  # minPts                 In principle minimum number of points in the unit disk, if the unit disk is within the cluster (core) [Ester et al., 1996, p. 228].
  #                        number of minimum points in the eps region (for core points). 
  #                      Default is 5 points.
  # OUTPUT List V with
  # Cls[1:n]               Clusterung der Daten, Points which cannot be assigned to a cluster will be reported as members of the noise cluster with NaN.
  # 
  # author: MT 2019
  
  if(is.null(nrow(Data))){# dann haben wir einen Vektor
    return(cls <- rep(1,length(Data)))
  }
  
  if(missing(Radius)){  
  requireNamespace('DataVisualizations')
    warning('The Radius (eps) parameter is missing but it is required in DBscan. Trying to estimate..')
    Radius=0.5*DataVisualizations::ParetoRadius(Data)
  } 
  if(missing(minPts)){
    minPts=min(round(0.0005*nrow(Data),2),20)## A point needs a least 16 (minPts) links in the sNN graph to be a core point.
    warning('The minPts parameter is missing but it is required in DBscan. Trying to estimate..')
  }   
  if(missing(UpperLimitRadius))
    UpperLimitRadius=2*Radius
  
  requireNamespace('dbscan',quietly = TRUE)
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
    requireNamespace('DataVisualizations')
    Cls2=Cls
    Cls2[Cls2==0]=999
    DataVisualizations::Plot3D(Data,Cls2)
  }
  
  return(list(Cls=Cls,Object=liste))
  
}
