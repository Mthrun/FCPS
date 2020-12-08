MarkovClustering=function(DataOrDistances=NULL,Adjacency=NULL,Radius=TRUE,DistanceMethod="euclidean",addLoops = TRUE,PlotIt=FALSE,...){
  #
  # INPUT
  # DataOrDistances[1:n,1:d]     DataOrDistances set with n observations and d features
  #
  # OPTIONAL
  # Adjacency         Used if DataOrDistances is missing, matrix [1:n,1:n] defining which points are adjacent
  #                   to each other by the number 1; not adjacent: 0
  # Radius            Radius for unit disk graph (r-ball graph) if adjacency matrix is missing.
  #                   Automatic estimation can be done either with =TRUE [Ultsch, 2005] or FALSE [Thrun et al., 2016]
  # addLoops          Logical; if TRUE, self-loops with weight 1 are added to each vertex of x (see MCL::mcl).
  # PlotIt            Boolean. Decision to plot or not
  #
  # OUTPUT
  # Cls[1:n]          Clustering of DataOrDistances
  # Object            Object of MCL::mcl algorithm
  #
  # Author: MT, 04/2018
  if (!requireNamespace('MCL',quietly = TRUE)) {
    message(
      'Subordinate clustering package (MCL) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(DataOrDistances)),
        Object = "Subordinate clustering package (MCL) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  if(!is.null(DataOrDistances)){
    
    if (!isSymmetric(unname(DataOrDistances))) {
 
    
      if(Radius==TRUE){
  		  if(requireNamespace('DataVisualizations',quietly = TRUE)){
  				Radius=DataVisualizations::ParetoRadius(DataOrDistances)
  			}else{
  				stop('DataVisualizations package is missing.')
  			}
        #Radius=AdaptGauss::ParetoRadius(DataOrDistances)
      }
      if(Radius==FALSE){
        if(requireNamespace('parallelDist',quietly = TRUE)){
    		  Radius=EstimateRadiusByDistance(as.matrix(parallelDist::parallelDist(DataOrDistances)))
    	  }else{
  		    stop('parallelDist package is missing.')
  	    }
      }
      
      if(requireNamespace('parallelDist',quietly = TRUE)){
        Distances=parallelDist::parDist(DataOrDistances,method=DistanceMethod)
      }else{
        Distances=dist(DataOrDistances,method=DistanceMethod)
      }
    }else{
      Distances=DataOrDistances
    }
    DistanceMatrix = as.matrix(Distances)
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
  # Distance=DistanceMatrix(FCPS$Hepta$DataOrDistances)
  # adjacency=KNNGraph(Distance,11)
  
  Cls=as.numeric(mm$Cluster)
  
  if(!is.null(rownames(DataOrDistances)))
    names(Cls)=rownames(DataOrDistances)
  else
    names(Cls)=1:nrow(DataOrDistances)
  
  if(PlotIt&!is.null(DataOrDistances)){
    ClusterPlotMDS(DataOrDistances,Cls)
  }
  	Cls=ClusterRename(Cls,DataOrDistances)
  return(list(Cls=Cls,Object=mm))
}
