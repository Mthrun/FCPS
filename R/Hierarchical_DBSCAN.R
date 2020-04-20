Hierarchical_DBSCAN <-function(Data,minPts=4,PlotTree=FALSE,PlotIt=FALSE,...){
  # Cls=Hierarchical_DBSCAN(FCPS$Hepta$Data,minPts=3)
  # DBscan nach  [Campello et al., 2015]
  # INPUT
  # Data[1:n,1:d]          der Datensatz 
  # minPts                 In principle minimum number of points in the unit disk, if the unit disk is within the cluster (core) [Ester et al., 1996, p. 228].
  #                        number of minimum points in the eps region (for core points). 
  # Optional
  #PlotTree
  #PlotIt
  # OUTPUT List V with
  # Cls[1:n]               Clusterung der Daten, Points which cannot be assigned to a cluster will be reported as members of the noise cluster with NaN.
  # Object
  #
  # author: MT2019
  #
  # [Campello et al., 2015]  Campello RJGB, Moulavi D, Zimek A, Sander J: Hierarchical density estimates for data clustering, visualization, and outlier detection, ACM Transactions on Knowledge Discovery from Data (TKDD), 10(5), pp. 1-51, 2015.
  
  
  
  if(is.null(nrow(Data))){# dann haben wir einen Vektor
    return(cls <- rep(1,length(Data)))
  }

  if(missing(minPts)){
    minPts=round(0.04*nrow(Data),0)
    warning('The minPts parameter is missing but it is required in DBscan. Trying to estimate. PlotTree is set to TRUE, please look at the dendrogram...')
    PlotTree=TRUE
  }   
  
  requireNamespace('dbscan')
  liste=dbscan::hdbscan(x = Data,minPts = minPts,...)
  Cls=liste$cluster
  ind=which(Cls==0)
  # if(length(ind)>0)
  #   Cls[ind]=999
  #Cls=NormalizeCls(Cls)$normalizedCls
  #if(length(ind)>0)
  #  Cls[ind]=NaN
  Cls[!is.finite(Cls)]=0
  #Per Definition are not clustered objects in searching for
  #distance and density based structures not allowed.
  #calling recursively
  #in case of outliers wie have a boundary of 5% of objects
  if(isTRUE(PlotTree)){
    plot(liste)
  }
  
  
  if(isTRUE(PlotIt)){
    Cls2=Cls
    Cls2[Cls2==0]=999
	ClusterPlotMDS(Data,Cls2)
  }
   Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=liste))
  
}
