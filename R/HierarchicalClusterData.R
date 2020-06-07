HierarchicalClusterData=HierarchicalCluster <-function(Data,ClusterNo=0,method="ward.D2",DistanceMethod="euclidean",ColorTreshold=0,Fast=FALSE,Cls=NULL,...){
  # HierarchicalClusterData(Data)
  # HierarchicalClusterDists(Data,0,"ward.D2",NULL,"cosine",100)
  # Cls=HierarchicalCluster(Data,6,"ward.D2")
  #  
  # Draws either a dendrogram or returns a class assignment
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  #
  # OPTIONAL
  # ClusterNo         Number of clusters to search for. If ClusterNo=0, then dendrogram will be plotted.
  # method  		      Cluster method: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".
  # DistanceMethod          see  DistanceMatrix(), for example 'euclidean','sqEuclidean','mahalanobis','cityblock=manhatten','cosine','chebychev','jaccard','minkowski','manhattan','binary', 'canberra', 'maximum'. Any unambiguous substring can be given.
  # ColorTreshold			Draws intersection at appropriate dendrogram y-axes (heigth), height of line is number
  # ...               Only if ClusterNo=0, arugments for plot
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Dendrogram        
  # Object      Object of hclust algorithm
  #
  # 
  # Author: MT
  # 1.Editor: MT 07/2015, pdist used for distance computation
  # Example
  # HierarchicalClusterDists(Data,0,"ward.D",NULL,"cosine",100, leaflab="none")  
  # NOTE:
  #Ward algorithm is directly correctly implemented in just Ward2, but rather that: (1) to get correct results with both implementations, use squared Euclidean distances with Ward1 and nonsquared Euclidean distances with Ward2; (2) to further make their output dendrograms comparable (identical), apply sq. root to fusion levels after Ward1 or square fusion levels after Ward2, before constructing dendrogram. 
 
  
  #Distanzberechnung
  #if (Distance=="euclidean"){
  #	if(any(is.nan(Data),na.rm=TRUE)) {
  #		Liste = naneucliddist(Data);
  ##		Y <- as.dist(Liste$distanceMatrix); #Umwandlung in Format, welches hc-clust akzeptiert
  #    warning('NaNs in Data excluded!')
  #	} 
  #	else{
  #		Y =dist(Data,method=Distance) #Fehler in pmatch(method, "euclidian") : Objekt 'Distance' nicht gefunden
  #	}
  #}
  #else{
  
  dcls = Cls
  
  if(requireNamespace("DataVisualizations")){
    col = DataVisualizations::DefaultColorSequence
  }
  else{
    stop('DataVisualizations package not loaded or installed.')
  }
  
  setNodeAttributes <- function(node){
    classification = -1
    # Stop condition
    if(is.leaf(node)){
      val <- node[1]
      classification = dcls[val]
      attr(node, "edgePar") <- list(col = col[classification])
      return(list(node=node, class=classification))
    }
  
    left <- setNodeAttributes(node[[1]])
    right <- setNodeAttributes(node[[2]])
    height = attr(node, "height")
    
    # Determine new class
    if((left$class == right$class)&(left$class != -1)) classification = left$class
    
    # Merge both children nodes
    node <- merge(left$node,right$node, height=height)
    if(classification != -1) 
      attr(node, "edgePar") <- list(col = col[classification])
  
    return(list(node=node,class=classification))
  }
  
  requireNamespace('parallelDist')
    Y=parallelDist::parDist(Data,method=DistanceMethod)#}    # Case Corr und otherwise
  if(any(is.nan(Y),na.rm=TRUE)) {
  stop('DistanceMethod with NaN in calculated. Please choose another distance.')}
  if(any(is.infinite(Y),na.rm=TRUE)) {
  stop('DistanceMethod with infinites in calculated. Please choose another distance.')}
  # Clustering
    
    if(isTRUE(Fast)&requireNamespace('fastcluster')){
      hc <- fastcluster::hclust(Y,method=method)
    }else{
      hc <- hclust(Y,method=method); # Returns partly different values than Z = linkage(Y,method);
    }
  if(DistanceMethod=='euclidean')
    DistanceMethod='Euclidean'
    
  m=paste(method,"LinkCluster/ ",DistanceMethod," N=",nrow(as.matrix(Data)))
  # Classification or dendrogram
  if (ClusterNo>0){
    Cls=cutree(hc,ClusterNo)
    Cls=ClusterRename(Cls,Data)
    return(list(Cls=Cls,Dendrogram=as.dendrogram(hc),Object=hc))
  } 
  else{
		x=as.dendrogram(hc)
		if(!is.null(Cls)){
  		x = setNodeAttributes(x)$node
  		print("Class colors:")
  		for(i in unique(Cls)){
  		  print(paste0("Class ", i, ": ", col[i]))
  		}
		}
		# # leaflab         a string specifying how leaves are labeled. The default "perpendicular" write text vertically (by default).
		#                   "textlike" writes text horizontally (in a rectangle), and 
		#                   "none" suppresses leaf labels 
		#                   s. ?as.dendrogramm
    #plot(x, main=m,xlab="Anzahl N", ylab=DistanceMethod, sub=" ",leaflab ="none")
    plot(x, main=m,xlab="Number of Data Points N", ylab=DistanceMethod, sub=" ", leaflab="none",...)
   # if(is.null(rownames(x))){
   #   plot(x, main=m,xlab="Anzahl N", ylab=DistanceMethod, sub=" ",leaflab ="none")
   # }else{
   #   plot(x, main=m,xlab="Anzahl N", ylab=DistanceMethod, sub=" ",leaflab =rownames(x))
   # }
		axis(1,col="black",las=1)
    if (ColorTreshold!=0){
			rect.hclust(hc, h=ColorTreshold,border="red")}
		else{
		#rect.hclust(hc, h=4*mean(hc$height),border="red")
		}
		return(list(Cls=NULL,Dendrogram=x,Object=hc))
	}
}
