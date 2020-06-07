ClusterInterDistances=InterClusterDistances=function(FullDistanceMatrix,Cls,Names,PlotIt=FALSE){
  #
  # INPUT
  # FullDistanceMatrix    symmetric distance matrix
  # Cls                   numerical vector of k classes
  #
  # OPTIONAL
  # Names                 character vector naming k classes
  # PlotIt                Boolean
  # 
  # OUTPUT
  # Matrix of k clusters, each columns consists of the distances between a cluster and all other clusters,
  # filled up with NaN at the end to be of the same lenght as the complete distance matrix.
  # 
  #
  u=sort(unique(Cls))
  classdist=list(FullDistanceMatrix[upper.tri(FullDistanceMatrix,diag = F)])
  if(length(u)==1) return(unlist(classdist))
  # Does not work for clusters with one point
  for(i in u){
    classdistcur=FullDistanceMatrix[Cls==i,Cls!=i]
    #if(i==1) print(classdistcur)
    distvec=classdistcur[upper.tri(classdistcur,diag = F)]
    classdist=c(classdist,list(distvec))
  }
  
  if(requireNamespace("DataVisualizations")){
    xmat=do.call(DataVisualizations::CombineCols,classdist)
  }
  else{
    stop('DataVisualizations package not loaded or installed.')
  }
  
  
  if(missing(Names)){
    colnames(xmat)=c('Full',paste0('Cluster',u))
  }else{
    if(length(u)!=length(Names)){
      warning('Length of Names has to be equal of length of unique Cls.')
      colnames(xmat)=c('Full',paste0('Cluster',Names))
    }else{
      colnames(xmat)=c('Full',Names)
    }
  }
  
   if(PlotIt){
      ggobject=DataVisualizations::MDplot(xmat,OnlyPlotOutput = TRUE)
      print(ggobject)
      return(list(ClusterDists=as.matrix(xmat),ggobject=ggobject))
   }
	
  return(as.matrix(xmat))
}