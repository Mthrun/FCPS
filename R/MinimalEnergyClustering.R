MinimalEnergyClustering <-function(DataOrDistances,ClusterNo=0,DistanceMethod="euclidean",ColorTreshold=0,Data,...){
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
  
  if (!isSymmetric(unname(DataOrDistances))) {
    requireNamespace('parallelDist')
    pDist=as.dist(parallelDist::parDist(DataOrDistances,method=DistanceMethod))
  }else if(!inherits(DataOrDistances,'dist')){
    pDist=as.dist(DataOrDistances)
  }else{
    pDist=DataOrDistances
  }
	hc <- energy::energy.hclust(pDist)
	m=paste("Minimal Energy Clustering/ "," N=",nrow(as.matrix(pDist)))
  # Classification or Dendrogram
	if (ClusterNo>0){
		Cls=cutree(hc,ClusterNo)
		Cls=ClusterRename(Cls,DataOrDistances)
		return(list(Cls=Cls,Dendrogram=as.dendrogram(hc),Object=hc))
	} 
	else{
		x=as.dendrogram(hc)
		plot(x, main=m,xlab="Number of Data Points N", ylab="Distance",sub=" ",leaflab ="none",...)
		axis(1,col="black",las=1)
		if (ColorTreshold!=0){
		  rect.hclust(hc, h=ColorTreshold,border="red")}		  
		else{
		}
		return(list(Cls=NULL,Dendrogram=x,Object=hc))
	}
}



