GenieClustering=function(DataOrDistances,ClusterNo=0,DistanceMethod="euclidean",ColorTreshold=0,...){

  if (!isSymmetric(unname(DataOrDistances))) {
    requireNamespace('parallelDist')
    pDist=as.dist(parallelDist::parDist(DataOrDistances,method=DistanceMethod))
  }else if(!inherits(DataOrDistances,'dist')){
    pDist=as.dist(DataOrDistances)
  }else{
    pDist=DataOrDistances
  }
  
  requireNamespace('genie')
  hc <- genie::hclust2(pDist,...)
  
  m=paste("Genie Clustering/ "," N=",nrow(as.matrix(pDist)))
  
  # Classification or Dendrogram
  if (ClusterNo>0){
	Cls=cutree(hc,ClusterNo)
    Cls=ClusterRename(Cls,DataOrDistances)
    return (Cls);
  } 
  else{
    x=as.dendrogram(hc);plot(x, main=m,xlab="Number of Data Points N", ylab="Distance",sub=" ",leaflab ="none")
    axis(1,col="black",las=1)
    if (ColorTreshold!=0){
      rect.hclust(hc, h=ColorTreshold,border="red")}		  
    else{
    }
    return(list(Cls=hc,Dedrogram=x))
  }
}