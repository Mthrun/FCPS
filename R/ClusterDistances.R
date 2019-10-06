ClusterDistances=function(FullDistanceMatrix,Cls,Names,PlotIt=FALSE){
  if(missing(Cls)){
    Cls=rep(1,nrow(FullDistanceMatrix))
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
    
    if(missing(Names)){
      colnames(xmat)=c('Full',paste0('Class',u))
    }else{
      if(length(u)!=length(Names)){
        warning('Lengh of Names has to be equal of length of unique Cls.')
        colnames(xmat)=c('Full',paste0('Class',Names))
      }else{
        colnames(xmat)=c('Full',Names)
      }
    }

  return(as.matrix(xmat))
}
