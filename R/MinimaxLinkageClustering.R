MinimaxLinkageClustering=function(DataOrDistances,ClusterNo=0,DistanceMethod="euclidean",ColorTreshold=0,...){
  # INPUT
  # DataOrDistances[1:n,1:d]    Dataset with n observations and d features or distance matrix with size n
  # ClusterNo                   Number of clusters to search for
  #
  # OPTIONAL
  # DistanceMethod    Choose distance metric.
  # ColorTreshold     draws cutline w.r.t. dendogram y-axis (height), height of line as scalar should be given
  # 
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Dendrogram
  # Object            Object of protoclust::protoclust algorithm
  #
  # Author: MT
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
    if(requireNamespace("parallelDist")){
      pDist=as.dist(parallelDist::parDist(DataOrDistances,method=DistanceMethod))
    }
    else{
      stop('parallelDist package not loaded or installed.')
    }
    
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
    return(list(Cls=Cls,Dendrogram=as.dendrogram(hc),Object=out))
  } 
  else{
    x=as.dendrogram(hc);plot(x, main=m,xlab="Number of data points N", ylab="Distance",sub=" ",leaflab ="none",...)
    axis(1,col="black",las=1)
    if (ColorTreshold!=0){
      rect.hclust(hc, h=ColorTreshold,border="red")}		  
    else{
    }
    return(list(Cls=NULL,Dendrogram=x,Object=hc))
  }
}