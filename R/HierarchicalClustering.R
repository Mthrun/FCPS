HierarchicalClustering=function(DataOrDistances,ClusterNo,method='SingleL',Fast=TRUE,Data,...){
  if(missing(DataOrDistances)){
    DataOrDistances=Data
  }
	if(missing(ClusterNo)) ClusterNo=0
  #unification for paper
  if(method=='SingleL') method="single"
  if(method=="Ward") method="ward.D2"
  if(method=='CompleteL') method="complete"
  if(method=='AverageL') method="average"
  if(method=='WPGMA') method="mcquitty"
  if(method=='MedianL') method="median"
  if(method=='CentroidL') method="centroid"
  
  #backwards compatibility to matlab, otherwise could be programmed better :-(
  if(method=='MinEnergy'){
    return(MinimalEnergyClustering(DataOrDistances = DataOrDistances,ClusterNo = ClusterNo,...))
  }else if(method=="Gini"){
    return(GenieClustering(DataOrDistances = DataOrDistances,ClusterNo = ClusterNo,...))
  }else if(method=="Minimax"){
    return(MinimaxLinkageClustering(DataOrDistances = DataOrDistances,ClusterNo = ClusterNo,...))
  }
  else if (isSymmetric(unname(DataOrDistances))) {
    if(!inherits(DataOrDistances,'dist')){
      Input=as.dist(DataOrDistances)
    }else{
      Input=DataOrDistances
    }
    return(HierarchicalClusterDists(pDist = Input,ClusterNo = ClusterNo,method = method,Fast=Fast,...))
  }else{#data given
    return(HierarchicalClusterData(Data = DataOrDistances,ClusterNo = ClusterNo,method = method,Fast=Fast,...))
  }#endisSymmetric(DataOrDistances)
  
}