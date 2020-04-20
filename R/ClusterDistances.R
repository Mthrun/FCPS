ClusterDistances=IntraClusterDistances=ClusterIntraDistances=function(FullDistanceMatrix,Cls,Names,PlotIt=FALSE){
  if(missing(Cls)){
    Cls=rep(1,nrow(FullDistanceMatrix))
  }
  if(!is.vector(Cls)){
    warning('ClusterDistances: Cls is not a vector. Calling as.numeric(as.character(Cls))')
    Cls=as.numeric(as.character(Cls))
  }
  
  if(nrow(FullDistanceMatrix)!=length(Cls)){
	stop('ClusterDistances: Dimensionality of Distance Matrix "FullDistanceMatrix" is not consistent with "Cls" classification vector')
  }
  
  if(!isSymmetric(unname(FullDistanceMatrix))){
	stop('ClusterDistances: Distance Matrix "FullDistanceMatrix" is not symmetric. Please check this, e.g. DataVisualizations::Pixelmatrix.')
  }
  u=sort(unique(Cls))

  classdist=list(FullDistanceMatrix[upper.tri(FullDistanceMatrix,diag = F)])
  if(length(u)==1) return(unlist(classdist))
  #funktioniert nicht bei clustersize==1!
  for(i in u){
    classdistcur=FullDistanceMatrix[Cls==i,Cls==i]
    distvec=classdistcur[upper.tri(classdistcur,diag = F)]
    classdist=c(classdist,list(distvec))
  }
  #addcols=function(...){
  #  return(rowr::cbind.fill(...,fill = NaN))
  #}
  

    xmat=do.call(DataVisualizations::CombineCols,classdist)
    
    if(missing(Names)){
      colnames(xmat)=c('Full',paste0('Class',u))
    }else{
      if(length(u)!=length(Names)){
        warning('ClusterDistances: Lengh of Names has to be equal of length of unique Cls.')
        colnames(xmat)=c('Full',paste0('Class',Names))
      }else{
        colnames(xmat)=c('Full',Names)
      }
    }
    
    if(PlotIt){
      ggobject=DataVisualizations::MDplot(xmat,Scaling = 'CompleteRobust')$ggplotObj
      print(ggobject)
      return(list(ClusterDists=as.matrix(xmat),ggobject=ggobject))
    }

  return(as.matrix(xmat))
}
