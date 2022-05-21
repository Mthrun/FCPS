ClusterRenameDescendingSize <- function(Cls,ProvideClusterNames=FALSE) {
  # Cls are renamed such that largest class =1 ...
  # RenamedCls = ClusterRenameDescendingSize(GivenCls)
  # 
  # INPUT
  # Cls    Vector of classifications
  # 
  # OUTPUT
  # RenamedCls    such that largest class =1 ...  
  #   
  # Author: MT, ALU
  if(!is.vector(Cls)){
    warning('ClusterRenameDescendingSize: Cls is not a vector. Calling as.numeric(as.character(Cls))')
    Cls=as.numeric(as.character(Cls))
  }
  if(length(unique(Cls))==1){
    warning("ClusterRenameDescendingSize: Only one unique label in Cls given. Nothing to rename.")
    return(list(renamedCls=Cls,ClusterName=NULL))
  }
  ListeV <- ClusterCount(Cls)
  countPerClass <- ListeV[[2]]
  UniqueClasses=ListeV[[1]]
  sortedClasses <- sort(na.last=TRUE,countPerClass, decreasing = TRUE, index.return=TRUE) # Original-Indizes mitliefern lassen
  numberOfClasses <- length(countPerClass)
  renamedCls <- Cls
  
  Matchingtable=matrix(0,numberOfClasses,2)
  Matchingtable[,1]=1:numberOfClasses
  colnames(Matchingtable)=c("New","Prior")
  for (i in 1: numberOfClasses) {
    Matchingtable[i,2]=UniqueClasses[sortedClasses$ix[i]]
    renamedCls[which(Cls == UniqueClasses[sortedClasses$ix[i]],arr.ind = T)] <- i # Hier mit den mitgelieferten Original-Indizes arbeiten
  } 
  if(isFALSE(ProvideClusterNames))
    return(renamedCls)
  else
    return(list(renamedCls=renamedCls,ClusterName=Matchingtable))
}
