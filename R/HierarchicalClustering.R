HierarchicalClustering=function(DataOrDistances,ClusterNo,method='SingleL',Data,...){
  if(missing(DataOrDistances)){
    DataOrDistances=Data
  }

  #unification for paper
  if(method=='SingleL') method="single"
  if(method=="Ward") method="ward.D2"
  if(method=='CompleteL') method="complete"
  if(method=='AverageL') method="average"
  if(method=='WPGMA') method="mcquitty"
  if(method=='Median') method="medianL"
  if(method=='CentroidL') method="centroid"
  
  #backwards compatibility to matlab, otherwise could be programmed better :-(
  if (isSymmetric(DataOrDistances)) {
    if(!inherits(DataOrDistances,'dist'))
      Input=as.dist(DataOrDistances)
    else
      Input=DataOrDistances
    
    if(method=='MinEnergy'){
      return(MinimalEnergyClustering(DataOrDistances = Input,ClusterNo = ClusterNo,...))
    }else{
      return(HierarchicalClusterDists(pDist = Input,ClusterNo = ClusterNo,method = method,...))
    }#end MinEnergy
  }else{#data given
    if(method=='MinEnergy'){
      
    }else{
      return(MinimalEnergyClustering(DataOrDistances = Input,ClusterNo = ClusterNo,...))
    }#end MinEnergy
  }#endisSymmetric(DataOrDistances)
  
}