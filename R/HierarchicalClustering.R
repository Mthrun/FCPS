HierarchicalClustering=function(DataOrDistances,ClusterNo,method='SingleL',Fast=TRUE,Data,...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features or distance matrix of size n
  # ClusterNo         Number of clusters to search for
  #
  # OPTIONAL
  # method            method of cluster analysis: "Ward", "SingleL", "CompleteL", "AverageL" (UPGMA),
  #                   "WPGMA" (mcquitty), "MedianL" (WPGMC), "CentroidL" (UPGMC), "Minimax", "MinEnergy",
  #                   "Gini" or "HDBSCAN".
  # Fast              Boolean. If TRUE and fastcluster installed, then a faster implementation of the methods
  #                   above can be used except for "Minimax", "MinEnergy", "Gini" or "HDBSCAN"
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of adpclust
  #
  # Author: MT, 04/2018
  if(missing(DataOrDistances)){
    DataOrDistances=Data
  }
	if(missing(ClusterNo)) ClusterNo=0
  # Unification for paper
  if(method=='SingleL') method="single"
  if(method=="Ward") method="ward.D2"
  if(method=='CompleteL') method="complete"
  if(method=='AverageL') method="average"
  if(method=='WPGMA') method="mcquitty"
  if(method=='MedianL') method="median"
  if(method=='CentroidL') method="centroid"
  
  # Backwards compatibility to matlab, otherwise could be programmed better :-(
  if(method=='MinEnergy'){
    return(MinimalEnergyClustering(DataOrDistances = DataOrDistances,ClusterNo = ClusterNo,...))
  }else if(method=="Gini"){
    return(GenieClustering(DataOrDistances = DataOrDistances,ClusterNo = ClusterNo,...))
  }else if(method=="Minimax"){
    return(MinimaxLinkageClustering(DataOrDistances = DataOrDistances,ClusterNo = ClusterNo,...))
  }else if(method=="HDBSCAN"){
    V=Hierarchical_DBSCAN(DataOrDistances = DataOrDistances,...)
    if(ClusterNo>1){
      Cls = cutree(V$Tree, ClusterNo)
    }else{
      Cls=ClusterDendrogram(V$Tree,1,Colorsequence = 'black',main = 'HDBSCAN Clustering')
    }
    return(list(Cls=Cls,Dendrogram=V$Dendrogram,Object=V$Tree,OriginalObject=V$Object))
  }
  else if (isSymmetric(unname(DataOrDistances))) {
    if(!inherits(DataOrDistances,'dist')){
      Input=as.dist(DataOrDistances)
    }else{
      Input=DataOrDistances
    }
    return(HierarchicalClusterDists(pDist = Input,ClusterNo = ClusterNo,method = method,Fast=Fast,...))
  }else{# Data given
    return(HierarchicalClusterData(Data = DataOrDistances,ClusterNo = ClusterNo,method = method,Fast=Fast,...))
  }#endisSymmetric(DataOrDistances)
  
}