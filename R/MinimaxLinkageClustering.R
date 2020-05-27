MinimaxLinkageClustering=function(DataOrDistances,ClusterNo=0,DistanceMethod="euclidean",ColorTreshold=0,...){
  
  if (!requireNamespace('protoclust')) {
    message(
      'Subordinate clustering package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(DataOrDistances)),
        Object = "Subordinate clustering package is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  if (!isSymmetric(unname(DataOrDistances))) {
    requireNamespace('parallelDist')
    pDist=as.dist(parallelDist::parDist(DataOrDistances,method=DistanceMethod))
  }else if(!inherits(DataOrDistances,'dist')){
    pDist=as.dist(DataOrDistances)
  }else{
    pDist=DataOrDistances
  }
  
  
  hc <- protoclust::protoclust(pDist,...)
  
  m=paste("Minimax Linkage Clustering/ "," N=",nrow(as.matrix(pDist)))
  
  # Classification or Dendrogram
  if (ClusterNo>0){
    out=protoclust::protocut(hc,ClusterNo)
	Cls=out$cl
	Cls=ClusterRename(Cls,DataOrDistances)
    return (list(Cls=Cls,Dendrogram=as.dendrogram(hc),Object=out))
  } 
  else{
    x=as.dendrogram(hc);plot(x, main=m,xlab="Number of Data Points N", ylab="Distance",sub=" ",leaflab ="none",...)
    axis(1,col="black",las=1)
    if (ColorTreshold!=0){
      rect.hclust(hc, h=ColorTreshold,border="red")}		  
    else{
    }
    return(list(Cls=NULL,Dendrogram=x,Object=hc))
  }
}