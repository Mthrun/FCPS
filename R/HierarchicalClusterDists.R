HierarchicalClusterDists <-function(pDist,ClusterNo=0,method="ward.D2",ColorTreshold=0,Fast=FALSE,...){
  # HierarchicalClusterDists(pDist)
  # HierarchicalClusterDists(pDist,0,"ward.D2",100)
  # Cls=HierarchicalClusterDists(pDist,6,"ward.D2")
  # 
  # Draws either a dendrogram or returns a class assignment
  # INPUT
  # pDist[1:n,1:d]    Dataset with n observations and d features or distance matrix with size n
  # ClusterNo        Number of clusters to search for. ClusterNo=0 means, that dendrogram will be used
  #
  # OPTIONAL
  # method			     Cluster method: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".
  # ColorTreshold		 Draws intersection at appropriate dendrogram y-axes (heigth), height of line is number
  # Fast             Enables a fast computation.
  # 
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Dendrogram        
  # hclustObject      Object of hclust algorithm
  # 
  # Author: MT
  # Clustering
  if(!inherits(pDist,'dist'))
    pDist=as.dist(pDist)
  
  if(isTRUE(Fast)&requireNamespace('fastcluster')){
    hc <- fastcluster::hclust(pDist,method=method)
  }else{
    hc <- hclust(pDist,method=method); #liefert teilweise andere Werte wie Z = linkage(Y,method);
  }
	m=paste(method,"LinkCluster/ "," N=",nrow(as.matrix(pDist)))
	
  # Classification or Dendrogram
	if (ClusterNo>0){
	Cls=cutree(hc,ClusterNo)
		return(list(Cls=Cls,Dendrogram=as.dendrogram(hc),Object=hc))
	} 
	else{
		x=as.dendrogram(hc);plot(x, main=m,xlab="Number of Data Points N", ylab="Distance",sub=" ",leaflab ="none",...)
		axis(1,col="black",las=1)
		if (ColorTreshold!=0){
		  rect.hclust(hc, h=ColorTreshold,border="red")}		  
		else{
		  #rect.hclust(hc, h=4*mean(hc$height),border="red")
		}
		return(list(Cls=NULL,Dendrogram=x,Object=hc))
	}
}



