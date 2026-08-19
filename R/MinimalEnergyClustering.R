MinimalEnergyClustering <-function(DataOrDistances,ClusterNo=0,ColorTreshold=0,DistanceMethod="euclidean",Data,PlotIt=FALSE,...){
  # HierarchicalClusterDists(pDist)
  # HierarchicalClusterDists(pDist,0,"ward.D2",100)
  # Cls=HierarchicalClusterDists(pDist,6,"ward.D2")
  #
  # Either draws dendrogram or returns class assignment
  #
  # INPUT
  # DataOrDistances[1:n,1:d]    Dataset with n observations and d features or distance matrix with size n
  #
  # OPTIONAL
  # ClusterNo         Number of clusters to search for. ClusterNo=0 means use of dendrogram
  # DistanceMethod    Choose distance metric.
  # ColorTreshold			Draws intersection at appropriate dendrogram y-ax (height). Height of line is number.
  # PlotIt            Boolean. Default = FALSE = No plotting performed.
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Dendrogram
  # Object            Object of energy::energy.hclust algorithm
  # 
  # Author: MT, 2019

  if (!requireNamespace('energy',quietly = TRUE)) {
    message(
      'Subordinate clustering package (energy) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(DataOrDistances)),
        Object = "Subordinate clustering package (energy) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  # Clustering
  if(missing(DataOrDistances)){
    DataOrDistances=Data
  }
  
  if(inherits(DataOrDistances,'dist')){
    pDist=DataOrDistances
  }else if (!IsDissimilarity(DataOrDistances)) {
    requireNamespace('parallelDist')
    pDist=as.dist(parallelDist::parDist(DataOrDistances,method=DistanceMethod))
  }else{
    pDist=as.dist(DataOrDistances)
  }
	hc <- energy::energy.hclust(pDist)
	plot_x_label =  sprintf("No. of Data Points N = %d",nrow(as.matrix(DataOrDistances)))
	m=sprintf("Minimum Energy clustering, k = %d", ClusterNo)
	x=as.dendrogram(hc)
  # Classification or Dendrogram
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
		  plot(x, main=m,xlab="Number of Data Points N", ylab="Distance",sub=" ",leaflab ="none",xlab=plot_x_label,...)
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
