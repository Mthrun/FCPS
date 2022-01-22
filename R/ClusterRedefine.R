ClusterRedefine=function(Cls,NewClusters,OldClusters){
  # 
  # INPUT
  # Cls                1:n numerical vector of numbers defining the classification as the main
  #                    output of the clustering algorithm for the n cases of data. It has k unique
  #                    numbers representing the arbitrary labels of the clustering.
  # NewClusters        [1:p], p<=k identifiers of clusters to be changed with
  #Optional
  # OldClusters    [1:p], p<=k identifiers of clusters to be changed, default [1:k] unique cluster Ids of cls
  # 
  # OUTPUT
  # Cls[1:n] numerical vector named after NewClusters 
  # 
    if(!is.vector(Cls)){
      warning('ClusterRedefine: Cls is not a vector. Calling as.numeric(as.character(Cls))')
      Cls=as.numeric(as.character(Cls))
    }
  if(missing(OldClusters))
    OldClusters=unique(Cls)
  
  if(length(OldClusters)!=length(NewClusters)){
    warning('ClusterRedefine: length(OldClusters)!=length(NewClusters))')
    return(Cls)
  }
  for(i in 1:length(OldClusters)){
    Cls[Cls==OldClusters[i]]=NewClusters[i]
  }
  
  return(Cls)
}