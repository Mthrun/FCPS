DBSCAN = DBscan=function(Data,Radius,minPts,Rcpp=TRUE,PlotIt=FALSE,UpperLimitRadius,...){
  # Cls=DBSCAN(FCPS$Hepta$Data,sqrt(min(res$withinss)))
  # DBSCAN based on [Ester et al., 1996]
  #
  # INPUT
  # Data[1:n,1:d]    Data set with n observations and d features
  # Radius           eps,  radius of R-ball [Ester et al., 1996, p. 227], size of the epsilon neighborhood.
  # 
  # OPTIONAL
  # minPts           In principle minimum number of points in the unit disk, if the unit disk is within the cluster (core) [Ester et al., 1996, p. 228].
  #                  number of minimum points in the eps region (for core points). 
  #                  Default is 5 points.
  # Rcpp              TRUE: uses rcpp fast version
  # PlotIt           Boolean. Decision to plot or not
  # UpperLimitRadius Limit for radius search, experimental
  #
  # OUTPUT
  # Cls[1:n]         Clustering of data. Points which cannot be assigned to a cluster will be reported as members of the noise cluster with NaN.
  # Object           Object of DBSCAN
  # 
  # Author: MT 2017, 1.Editor MT 04/2018, 2. Editor: MT 02/2019: Parameter Estimation significantly improved
  #
  # [Ester et al., 1996]  Ester, M., Kriegel, H.-P., Sander, J., & Xu, X.: A density-based algorithm for discovering clusters in large spatial databases with noise, Proc. Kdd, Vol. 96, pp. 226-231, 1996.

  if(isTRUE(Rcpp)){
    if (!requireNamespace('mlpack',quietly = TRUE)) {
      message(
        'Subordinate clustering package (mlpack) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
      )
      return(
        list(
          Cls = rep(1, nrow(Data)),
          Object = "Subordinate clustering package (mlpack) is missing.
                Please install the package which is defined in 'Suggests'."
        )
      )
    }
  }else{
    

  if (!requireNamespace('dbscan',quietly = TRUE)) {
    message(
      'Subordinate clustering package (dbscan) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate clustering package (dbscan) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  }
 if(is.null(nrow(Data))){# Then we get a vector
    return(cls <- rep(1,length(Data)))
  }
  
  if(is.null(Radius)){
    if(requireNamespace("DataVisualizations",quietly = TRUE)){
      warning('The Radius (eps) parameter is missing but it is required in DBSCAN. Trying to estimate..')
      Radius=0.5*DataVisualizations::ParetoRadius(Data)
    }
    else{
      stop('DataVisualizations package not loaded or installed.')
    }
  }
  if(is.null(minPts)){
    minPts=round(0.04*nrow(Data),0)
    warning('The minPts parameter is missing but it is required in DBSCAN. Trying to estimate..')
  }   
  if(missing(UpperLimitRadius))
    UpperLimitRadius=1.1*Radius

  if(isTRUE(Rcpp)){
    liste=mlpack::dbscan(input =  Data,epsilon =  Radius,min_size = minPts,...)
    Cls=as.vector(liste$assignments)
  }else{
    liste=dbscan::dbscan(x = Data,eps = Radius,minPts = minPts,...)
    Cls=liste$cluster
  }
  ind=which(Cls==0)
  Cls[!is.finite(Cls)]=0
  # Per Definition are not clustered objects in searching for
  # distance and density based structures not allowed.
  # Calling recursively
  # In case of outliers wie have a boundary of 5% of objects
  if(Radius<UpperLimitRadius&sum(Cls==0)>round(0.05*nrow(Data))){
    out=DBSCAN(Data,Radius*1.01,minPts,PlotIt,UpperLimitRadius=UpperLimitRadius,...)
    Cls=out$Cls
    liste=out$DBscanObject
  }
	if(isTRUE(PlotIt)){
	  Cls2=Cls
	  Cls2[Cls2==0]=999
	  p=ClusterPlotMDS(Data,Cls2)
	  print(p)
	}
	  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=liste))

}
