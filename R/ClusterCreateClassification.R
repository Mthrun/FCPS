ClusterCreateClassification=function(Objects){
  if(is.list(Objects)) Objects=unlist(Objects)
  y=as.character(Objects)
  n=length(y)
  u=unique(y,fromLast = FALSE)
  names(u)=1:length(u)
  Cls=rep(NaN,n)
  for(i in 1:length(u)){
    Cls[y==u[i]]=i
  }
  return(list(Cls=Cls,ClusterNames=u))
}