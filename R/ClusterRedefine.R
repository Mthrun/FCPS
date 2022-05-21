ClusterRedefine=function(Cls,NewLabels,OldLabels){
  # 
  # INPUT
  # Cls                1:n numerical vector of numbers defining the classification as the main
  #                    output of the clustering algorithm for the n cases of data. It has k unique
  #                    numbers representing the arbitrary labels of the clustering.
  # NewLabels        [1:p], p<=k identifiers of clusters to be changed with
  #Optional
  # OldLabels    [1:p], p<=k identifiers of clusters to be changed, default [1:k] unique cluster Ids of cls
  # 
  # OUTPUT
  # Cls[1:n] numerical vector named after NewLabels 
  # 
    if(!is.vector(Cls)){
      warning('ClusterRedefine: Cls is not a vector. Calling as.numeric(as.character(Cls))')
      Cls=as.numeric(as.character(Cls))
    }
  if(missing(OldLabels))
    OldLabels=unique(Cls)
  
  if(length(OldLabels)!=length(NewLabels)){
    warning('ClusterRedefine: length(OldLabels)!=length(NewLabels))')
    return(Cls)
  }
  if(length(unique(Cls))==1){
    warning("ClusterRedefine: Only one unique label in Cls given. Nothing to redfine.")
    return(Cls)
  }
  #store old clusters
  indV=list()
  for(i in 1:length(OldLabels)){#iterate cluster labels
    #stores boolean vector
    indV[[i]]=c(Cls==OldLabels[i])
  }
  #apply renaming but ignore same label in old cluster and new cluster that indicates different clusters!
  for(i in 1:length(OldLabels)){
    Cls[indV[[i]]]=NewLabels[i]
  }
  return(Cls)
}