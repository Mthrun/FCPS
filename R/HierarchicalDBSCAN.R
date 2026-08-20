HierarchicalDBSCAN =Hierarchical_DBSCAN=Hierarchical_DBscan=function(DataOrDistances,minPts=4,PlotTree=FALSE,PlotIt=FALSE,...){
  # Cls=HierarchicalDBSCAN(FCPS$Hepta$Data,minPts=3)
  # DBscan based on  [Campello et al., 2015]
  #
  # INPUT
  # DataOrDistances[1:n,1:d]    Dataset with n observations and d features or distance matrix with size n
  # minPts                      In principle minimum number of points in the unit disk, if the unit disk is
  #                             within the cluster (core) [Ester et al., 1996, p. 228].
  #                             Number of minimum points in the eps region (for core points). 
  # OPTIONAL
  # PlotTree          Boolean. Default: FALSE, If TRUE plots the dendrogram. If minPts is missing, PlotTree
  #                   is set to TRUE.
  # PlotIt            Boolean. Default: FALSE, If TRUE plots the first three dimensions of the dataset with
  #                   colored three-dimensional data points defined by the clustering stored in 
  #
  # OUTPUT
  # Cls[1:n]      Clustering of data. Points which cannot be assigned to a cluster will be reported as members
  #               of the noise cluster with NaN.
  # Dendrogram    Dendrogram of hierarchical clustering algorithm
  # Tree          Ultrametric tree of hierarchical clustering algorithm
  # Object        Object of hdbscan algorithm
  # 
  # author: MT2019
  #
  # [Campello et al., 2015]  Campello RJGB, Moulavi D, Zimek A, Sander J: Hierarchical density estimates for data clustering, visualization, and outlier detection, ACM Transactions on Knowledge Discovery from Data (TKDD), 10(5), pp. 1-51, 2015.
  
  if(missing(DataOrDistances) || is.null(DataOrDistances)){
    stop("HierarchicalDBSCAN: 'DataOrDistances' is required.", call. = TRUE)
  }

  n <- if(inherits(DataOrDistances, "dist")){
    attr(DataOrDistances, "Size", exact = TRUE)
  }else if(!is.null(nrow(DataOrDistances))){
    nrow(DataOrDistances)
  }else{
    length(DataOrDistances)
  }

  if(length(n)!=1L || is.na(n) || !is.finite(n) || n<1L){
    stop("HierarchicalDBSCAN: could not determine the number of observations.", call. = TRUE)
  }
  n <- as.integer(n)

  if (!requireNamespace('dbscan',quietly = TRUE)) {
    message(
      'Subordinate clustering package (dbscan) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, n),
        Object = "Subordinate clustering package (dbscan) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  if (IsDissimilarity(DataOrDistances)) {
    Data=stats::as.dist(DataOrDistances)
  }else if(is.null(dim(DataOrDistances))){
    Data=matrix(DataOrDistances,ncol=1L)
  }else{
    Data=DataOrDistances
  }

  if(is.null(minPts)){
    minPts=round(0.04*n,0)
    warning('The minPts parameter is missing but it is required in DBscan. Trying to estimate. PlotTree is set to TRUE, please look at the dendrogram...')
    PlotTree=TRUE
  }   
  

  liste=dbscan::hdbscan(x = Data,minPts = minPts,gen_hdbscan_tree=TRUE,gen_simplified_tree =TRUE,...)
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
	ClusterPlotMDS(DataOrDistances,Cls2)
  }
  RenameData <- if(inherits(DataOrDistances,"dist")){
    as.matrix(DataOrDistances)
  }else if(is.null(dim(DataOrDistances))){
    Data
  }else{
    DataOrDistances
  }
  Cls=ClusterRename(Cls,RenameData)
  return(list(Cls=Cls,Dendrogram=as.dendrogram(liste$hdbscan_tree),Tree=as.hclust(as.dendrogram(liste$hdbscan_tree)),Object=liste))
  
}
