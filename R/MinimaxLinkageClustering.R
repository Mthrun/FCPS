MinimaxLinkageClustering=function(DataOrDistances,ClusterNo=0,ColorTreshold=0,DistanceMethod="euclidean",PlotIt=FALSE,...){
  # INPUT
  # DataOrDistances[1:n,1:d]    Dataset with n observations and d features or distance matrix with size n
  # ClusterNo                   Number of clusters to search for
  #
  # OPTIONAL
  # DistanceMethod    Choose distance metric.
  # ColorTreshold     draws cutline w.r.t. dendogram y-axis (height), height of line as scalar should be given
  # PlotIt            Boolean. Default = FALSE = No plotting performed.
  # 
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Dendrogram
  # Object            Object of protoclust::protoclust algorithm
  #
  # Author: MT
  if (!requireNamespace('protoclust',quietly = TRUE)) {
    message(
      'Subordinate clustering package (protoclust) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(DataOrDistances)),
        Object = "Subordinate clustering package (protoclust) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  if(inherits(DataOrDistances,'dist')){
    pDist=DataOrDistances
  }else if (!isSymmetric(unname(as.matrix(DataOrDistances)))) {
    if(requireNamespace("parallelDist",quietly = TRUE)){
      pDist=as.dist(parallelDist::parDist(DataOrDistances,method=DistanceMethod))
    }
    else{
      warning("Please install the parallelDist package, using dist()")
      pDist=dist(DataOrDistances,method=DistanceMethod)
    }
  }else{
    pDist=as.dist(DataOrDistances)
  }
  
  hc <- protoclust::protoclust(pDist,...)
  
  m=sprintf("Minimax Linkage clustering, k = %d", ClusterNo)
  plot_x_label =  sprintf("No. of Data Points N = %d",nrow(as.matrix(DataOrDistances)))
  x=as.dendrogram(hc)
  
  # Classification or Dendrogram
  if(ClusterNo>0){
    out=protoclust::protocut(hc,ClusterNo)
	  Cls=out$cl
	  Cls=ClusterRename(Cls,DataOrDistances)
	  if(isTRUE(PlotIt)){
	    V=ClusterDendrogram(TreeOrDendrogram=x,ClusterNo=ClusterNo,main=m,xlab=plot_x_label)
	  }
    return(list(Cls=Cls,Dendrogram=x,Object=out))
  } 
  else{
    if(isTRUE(PlotIt)){
      plot(x, main=m,xlab="Number of data points N", ylab="Distance",sub=" ",leaflab ="none",xlab=plot_x_label,...)
      axis(1,col="black",las=1)
    }
    if (ColorTreshold!=0){
      if(isTRUE(PlotIt)) rect.hclust(hc, h=ColorTreshold,border="red")
      Cls=cutree(hc,h=ColorTreshold)
      Cls=ClusterRename(Cls,DataOrDistances)
    }else{
      Cls=NULL
    }
    return(list(Cls=Cls,Dendrogram=x,Object=hc))
  }
}
