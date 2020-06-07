OPTICSclustering=function(Data, MaxRadius,RadiusThreshold, minPts = 5, PlotIt=FALSE,...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # MaxRadius         upper limit neighborhood in the R-ball graph/unit disk graph), size of the
  #                   epsilon neighborhood  (eps) [Ester et al., 1996, p. 227]. If NULL, automatic
  #                   estimation is done using insights of [Ultsch, 2005].
  # RadiusThreshold   Threshold to identify clusters (RadiusThreshold <= MaxRadius), if NULL 0.9*MaxRadius is set.
  #
  # OPTIONAL
  # minPts            Default = 5
  # PlotIt            Boolean. Decision to plot or not
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of adpclust algorithm
  #
  # Author: MT, 04/2018
  if (!requireNamespace('dbscan')) {
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
  
  if(is.null(MaxRadius)){  
    warning('The MaxRadius (eps) parameter is missing but it is required in OPTICS. Trying to estimate..')
	  if(requireNamespace("DataVisualizations")){
	    MaxRadius=0.5*DataVisualizations::ParetoRadius(Data)
	  }else{
	    stop('DataVisualizations package not loaded or installed.')
	  }
  }
  if(is.null(RadiusThreshold)){  
    warning('The RadiusThreshold (eps_cl) parameter is missing but it is required in OPTICS. Trying to estimate..')
    RadiusThreshold=0.9*MaxRadius
  } 
  if(is.null(minPts)){
    minPts=round(0.025*nrow(Data),0)
    warning('The minPts parameter is missing but it is required in DBscan. Trying to estimate..')
  }  
  out=dbscan::optics(Data,eps=MaxRadius,minPts=minPts,...)
  OPTICScobject=dbscan::extractDBSCAN(out, eps_cl = RadiusThreshold)

  Cls=OPTICScobject$cluster
  Cls[!is.finite(Cls)]=0
  if(!is.null(rownames(Data))){
    names(Cls)=rownames(Data)
  }
  
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
	Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=OPTICScobject))
}
