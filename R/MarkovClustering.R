MarkovClustering=function(Data=NULL,Adjacency=NULL,Radius=TRUE,addLoops = TRUE,PlotIt=FALSE,...){
  #
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  #
  # OPTIONAL
  # Adjacency         Used if Data is missing, matrix [1:n,1:n] defining which points are adjacent
  #                   to each other by the number 1; not adjacent: 0
  # Radius            Radius for unit disk graph (r-ball graph) if adjacency matrix is missing.
  #                   Automatic estimation can be done either with =TRUE [Ultsch, 2005] or FALSE [Thrun et al., 2016]
  # addLoops          Logical; if TRUE, self-loops with weight 1 are added to each vertex of x (see MCL::mcl).
  # PlotIt            Boolean. Decision to plot or not
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of MCL::mcl algorithm
  #
  # Author: MT, 04/2018
  if (!requireNamespace('MCL')) {
    message(
      'Subordinate clustering package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate clustering package is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  if(!is.null(Data)){
    if(Radius==TRUE){
		  if(requireNamespace('DataVisualizations')){
				Radius=DataVisualizations::ParetoRadius(Data)
			}else{
				stop('DataVisualizations package is missing.')
			}
      #Radius=AdaptGauss::ParetoRadius(Data)
    }
    if(Radius==FALSE){
      if(requireNamespace('parallelDist')){
  		  Radius=EstimateRadiusByDistance(as.matrix(parallelDist::parallelDist(Data)))
  	  }else{
		    stop('parallelDist package is missing.')
	    }
    }
    DistanceMatrix = as.matrix(dist(Data))
    AnzPunkte = nrow(DistanceMatrix)
    N = ncol(DistanceMatrix)
    Adjacency = matrix(0, ncol = N, nrow = N)
    for (i in 1:AnzPunkte) {
      RInd = which(DistanceMatrix[i, ] <= Radius, arr.ind = T)
      Adjacency[i, RInd] = 1
    }
  }
  
  mm=MCL::mcl(x = Adjacency,addLoops =addLoops,...)

  # Choice necessary, therefore in automatization not usable
  # Distance=DistanceMatrix(FCPS$Hepta$Data)
  # adjacency=KNNGraph(Distance,11)
  
  Cls=as.numeric(mm$Cluster)
  
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
  else
    names(Cls)=1:nrow(Data)
  
  if(PlotIt&!is.null(Data)){
    ClusterPlotMDS(Data,Cls)
  }
  	Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=mm))
}
