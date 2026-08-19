GenieClustering=function(DataOrDistances,ClusterNo=0,ColorTreshold=0,DistanceMethod="euclidean",PlotIt=FALSE,...){
  # INPUT
  # DataOrDistances[1:n,1:d]    Dataset with n observations and d features or distance matrix with size n
  #
  # OPTIONAL
  # ClusterNo                   Number of clusters to search for
  # DistanceMethod              String. 'euclidean','mahalanobis','manhatten' (cityblock),'fJaccard','binary', 'canberra', 'maximum'
  # ColorTreshold               Number. Draws cutline w.r.t. dendogram y-axis (height), height of line as scalar should be given
  # PlotIt                      Boolean. Default = FALSE = No plotting performed.
  # 
  # OUTPUT
  # Cls[1:n]    Clustering of data
  # hc          Object of hclust2 algorithm
  #
  # Author: MT
  if (!requireNamespace('genie',quietly = TRUE)) {
    message(
      'Subordinate clustering package (genie) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(DataOrDistances)),
        Object = "Subordinate clustering package (genie) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  if(inherits(DataOrDistances,'dist')){
    pDist=DataOrDistances
  }else if (!IsDissimilarity(DataOrDistances)) {
    if(requireNamespace("parallelDist",quietly = TRUE)){
      pDist=as.dist(parallelDist::parDist(DataOrDistances,method=DistanceMethod))
    }
    else{
      stop('parallelDist package not loaded or installed.')
    }
  }else{
    pDist=as.dist(DataOrDistances)
  }
  
  #requireNamespace('genie')
  hc <- genie::hclust2(pDist,...)
  

  
  m=sprintf("Genie clustering, k = %d", ClusterNo)
  x=as.dendrogram(hc)
  plot_x_label =  sprintf("No. of Data Points N = %d",nrow(as.matrix(DataOrDistances)))
  # Classification or dendrogram
  if (ClusterNo>0){
	  Cls=cutree(hc,ClusterNo)
    Cls=ClusterRename(Cls,DataOrDistances)
    if(isTRUE(PlotIt)){
      V=ClusterDendrogram(TreeOrDendrogram=x,ClusterNo=ClusterNo,main=m,xlab=plot_x_label)
    }
    return(list(Cls=Cls,Dendrogram=x,Object=hc))
  } 
  else{
    if(isTRUE(PlotIt)){
      plot(x, main=m,xlab=plot_x_label, ylab="Distance",sub=" ",leaflab ="none")
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
