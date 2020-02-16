InterClusterDistances=function(FullDistanceMatrix,Cls,Names,PlotIt=FALSE){
  u=sort(unique(Cls))
  classdist=list(FullDistanceMatrix[upper.tri(FullDistanceMatrix,diag = F)])
  if(length(u)==1) return(unlist(classdist))
  #Funktioniert nicht wenn in custer genau 1 punkt
  for(i in u){
    classdistcur=FullDistanceMatrix[Cls==i,Cls!=i]
    #if(i==1) print(classdistcur)
    distvec=classdistcur[upper.tri(classdistcur,diag = F)]
    classdist=c(classdist,list(distvec))
  }
  #addcols=function(...){
  # return(rowr::cbind.fill(...,fill = NaN))
  #}
  
  xmat=do.call(DataVisualizations::CombineCols,classdist)
  
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