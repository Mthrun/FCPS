kmeansDist <- function(Distance, ClusterNo=2,Centers=NULL,RandomNo=1,maxIt = 2000, PlotIt=FALSE,verbose = F){
  message('kmeansDist function is still in test phase. Only experimental version usable.')
  # KMeans version using ONLY datapoints as centers
  #
  # INPUT
  # Data[1:n,1:d]    Data set with n observations and d features
  # ClusterNo        Number of clusters to search for
  #
  # OPTIONAL
  # Centers       Vector with initial centers. All centers must be data points
  # RandomNo      Only for Steinley method or in case of distance matrix, number of random initializations with
  #               searching for minimal SSE, see [Steinley/Brusco, 2007]
  # maxIt         Max number of iterations
  # PlotIt        Boolean. Decision to plot or not
  # verbose       Enables/Disables (TRUE/FALSE) text output during process. Default: FALSE
  #
  # OUTPUT
  # Cls[1:n]     Clustering of data
  # centerids    Centroids
  # SSE          Sum of squared errors

  if(!is.null(Centers))  
    centers= Centers  # fuer Backward Compatibility
  else
    centers=ClusterNo
  
  if(!is.finite(RandomNo)) RandomNo=1
  
  if(RandomNo<1) RandomNo=1
  
  kstepdist <- function(centers, dists){
    dists2centers = dists[centers,]
    idclusts = apply(dists2centers, 2, which.min)
    SSEl=list()
    for(i in 1:length(centers)){ # F?r jedes Cluster
      # Welche Punkte liegen im Cluster
      inClust = which(idclusts == i)
      inClustDist = dists[inClust,inClust]
      # Betrachte nur distanzen innerhalb des Clusters
      if(length(inClust)>1){
        # means!
        SSE=rowMeans(inClustDist)
      }else{
        SSE=mean(inClustDist)
      }
      centers[i] = inClust[which.min(SSE)]
      SSEl[[i]]=SSE
    }
    SSE=sapply(SSEl, sum)
    return(list(centerids=centers, nClusts=idclusts,SSE=SSE))
  }#end kstepdist

  
  kmeansdistpertrail=function(Distance, centers,maxIt = 2000,verbose = F){
  if(!is.vector(centers)){
    warning("Centers is expected to be a vector of indices")
    return
  }
  
  if(length(centers) == 1){
    centerids = sample(dim(Distance)[1],centers)
  } else {
    centerids = centers
  }
  converged = F
  clusts = NULL
  nClusts = NULL
  i = 0
  while(!converged){
    i <- i+1
	if(i > maxIt){
      converged = T
      print("Warning, did not converge")
    }
	
    if(verbose)
      print(paste('Starting iteration', toString(i)))

    if(!is.null(clusts) && all(nClusts==clusts)){
      converged <- T
    } else {
      clusts <- nClusts
    }
    step = kstepdist(centerids, Distance)
    centerids = step$centerids
    nClusts = step$nClusts
    SSE=step$SSE
    
  }
  return(list(Cls=nClusts,centerids=centerids,SSE=SSE))
  }#end kmeansdistpertrail
  

  Liste=lapply(1:RandomNo, function(i,Distance, centers,maxIt,verbose){
    onetrial=NULL
    SSE=NaN
    try({onetrial=kmeansdistpertrail(Distance, centers,maxIt,verbose )
    SSE=sum( onetrial$SSE)
    })

    return(list(SSE,kmeansOut=onetrial))
  },Distance,centers,maxIt,verbose)

  
  SSEs=unlist(lapply(Liste, "[[",1))

  res=Liste[[which.min(SSEs)]]$kmeansOut

  Cls=as.vector(res$Cls)
  
  Cls=ClusterRename(Cls,Distance)
  
  if(PlotIt){
    ClusterPlotMDS(Distance,Cls)
  }

  return(list(Cls=Cls,centerids=res$centerids,SSE=res$SSE))
}

