HierarchicalClustering=function(DataOrDistances,ClusterNo,Type='SingleL',Fast=TRUE,Data,...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features or distance matrix of size n
  # ClusterNo         Number of clusters to search for
  #
  # OPTIONAL
  # Type            Type of cluster analysis: "Ward", "SingleL", "CompleteL", "AverageL" (UPGMA),
  #                   "WPGMA" (mcquitty), "MedianL" (WPGMC), "CentroidL" (UPGMC), "Minimax", "MinEnergy",
  #                   "Gini" or "HDBSCAN".
  # Fast              Boolean. If TRUE and fastcluster installed, then a faster implementation of the Types
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
  if(Type=='SingleL') Type="single"
  if(Type=="Ward") Type="ward.D2"
  if(Type=='CompleteL') Type="complete"
  if(Type=='AverageL') Type="average"
  if(Type=='WPGMA') Type="mcquitty"
  if(Type=='MedianL') Type="median"
  if(Type=='CentroidL') Type="centroid"
  
  # Backwards compatibility to matlab, otherwise could be programmed better :-(
  if(Type=='MinEnergy'){
    return(MinimalEnergyClustering(DataOrDistances = DataOrDistances,ClusterNo = ClusterNo,...))
  }else if(Type=="Gini"){
    return(GenieClustering(DataOrDistances = DataOrDistances,ClusterNo = ClusterNo,...))
  }else if(Type=="Minimax"){
    return(MinimaxLinkageClustering(DataOrDistances = DataOrDistances,ClusterNo = ClusterNo,...))
  }else if(Type=="HDBSCAN"){
    V=Hierarchical_DBSCAN(DataOrDistances = DataOrDistances,...)
    if(ClusterNo>1){
      Cls = cutree(V$Tree, ClusterNo)
    }else{
      #ClusterDendrogram(V$Tree,1,Colorsequence = 'black',main = 'HDBSCAN Clustering')
      Cls=V$Cls#automatic number of clusters selection by Hierarchical_DBSCAN
    }
    return(list(Cls=Cls,Dendrogram=V$Dendrogram,Object=V$Tree,OriginalObject=V$Object))
  }
  else if (isSymmetric(unname(DataOrDistances))) {
    if(!inherits(DataOrDistances,'dist')){
      Input=as.dist(DataOrDistances)
    }else{
      Input=DataOrDistances
    }
    return(HierarchicalClusterDists(pDist = Input,ClusterNo = ClusterNo,Type = Type,Fast=Fast,...))
  }else{# Data given
    return(HierarchicalClusterData(Data = DataOrDistances,ClusterNo = ClusterNo,Type = Type,Fast=Fast,...))
  }#endisSymmetric(DataOrDistances)
  
}