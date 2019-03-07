ClusterDistances=function(FullDistanceMatrix,Cls,PlotIt=FALSE){
  if(missing(Cls)){
    Cls=rep(1,nrow(FullDistanceMatrix))
  }
  u=unique(Cls)
  classdist=list(FullDistanceMatrix[upper.tri(FullDistanceMatrix,diag = F)])
  if(length(u)==1) return(unlist(classdist))
  
  for(i in u){
    classdistcur=FullDistanceMatrix[Cls==i,Cls==i]
    distvec=classdistcur[upper.tri(classdistcur,diag = F)]
    classdist=c(classdist,list(distvec))
  }
  addcols=function(...){
    return(rowr::cbind.fill(...,fill = NaN))
  }
  
  # if(PlotIt){
  #   ggobject=MDplot4multiplevectors(unlist(classdist))$ggplotObj
  #   xmat=do.call(addcols,classdist)
  #   colnames(xmat)=c('Full',paste0('Class',u))
  #   print(ggobject)
  #   return(list(ClusterDists=as.matrix(xmat),ggobject=ggobject))
  # 
  # }
    xmat=do.call(addcols,classdist)
    colnames(xmat)=c('Full',paste0('Class',u))
  return(as.matrix(xmat))
}
