ClusterCreateClassification=function(Objects,Decreasing){
  #
  # INPUT
  # Objects          listed objects, for example factor
  #
  # OUTPUT
  # Cls[1:n]         Clustering of data
  # ClusterNames     Object of adpclust algorithm
  # Decreasing        if not missing, objects are sorted
  # Author: MT
  if(is.list(Objects)) Objects=unlist(Objects)
  y=as.character(Objects)

  n=length(y)
  u=unique(y,fromLast = FALSE)
  if(!missing(Decreasing)){
    u=sort(u,decreasing = Decreasing,na.last=TRUE)
  }
  names(u)=1:length(u)
  Cls=rep(NaN,n)
  for(i in 1:length(u)){
    Cls[y==u[i]]=i
  }

  return(list(Cls=Cls,ClusterNames=u))
}