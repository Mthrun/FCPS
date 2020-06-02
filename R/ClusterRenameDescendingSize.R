ClusterRenameDescendingSize <- function(Cls) {
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
  
  ListeV <- ClusterCount(Cls)
  countPerClass <- ListeV[[2]]
  UniqueClasses=ListeV[[1]]
  sortedClasses <- sort(na.last=TRUE,countPerClass, decreasing = TRUE, index.return=TRUE) # Original-Indizes mitliefern lassen
  numberOfClasses <- length(countPerClass)
  renamedCls <- Cls
  
  for (i in 1: numberOfClasses) {
    renamedCls[which(Cls == UniqueClasses[sortedClasses$ix[i]],arr.ind = T)] <- i # Hier mit den mitgelieferten Original-Indizes arbeiten
  }  
  return(renamedCls)
}
