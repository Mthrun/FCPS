PFclustering=function(Data,ClusterNo,Fuzzifier=2,DistanceMethod= "Euclidean",NoIterations=100,PlotIt=FALSE,...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Number of clusters to search for
  #
  # OPTIONAL
  # Fuzzifier      Numeric scalar > 1. Fuzzifier parameter m. Default: 2.
  # DistanceMethod Character scalar. Distance metric to use.
  #                Supported values according to the documentation:
  #                "Minkowski", "Mahalanobis", or "Euclidean".
  # NoIterations   Integer scalar. Maximum number of iterations. Default: 100.
  # ...            Further parameters passed to \code{pfclust::PFC()}, depending on
  #                the distance choice, e.g.:
  #                - q         : distance exponent, must be > 0 (default often 2)
  #                - p         : Minkowski exponent
  #                - alpha,beta: Mahalanobis parameters
  #                - threshold : convergence tolerance
  #
  # PlotIt            Boolean. Decision to plot or not
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of pfclust
  #
  # Author: MT, 07/2026
  #reference: P. T. Nguyen, C. Tortora and A. Punzo, "Power Fuzzy Clustering: Flexible Distance Metrics and Inclusion of Covariates," in IEEE Transactions on Fuzzy Systems, vol. 34, no. 7, pp. 2171-2182, July 2026, doi: 10.1109/TFUZZ.2026.3683998.
  
  if (!requireNamespace('pfclust', quietly = TRUE)) {
    message(
      'Subordinate clustering package (pfclust) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate clustering package (pfclust) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  if (ClusterNo < 2) {
    warning("ClusterNo should be an integer > 2. Now, all of your data is in one cluster.")
    if (is.null(nrow(Data))) {
      # then we got a vector
      return(list(Cls = rep(1, length(Data)),Object=NULL))
    } else{
      # Matrix
      return(list(Cls = rep(1, nrow(Data)),Object=NULL))
    }
  }
  
  res=pfclust::PFC(Y = Data,K=ClusterNo,m=Fuzzifier,distance = DistanceMethod,max.iter=NoIterations,...)
  
  Cls=as.numeric(res$l)
  Cls=ClusterRename(Cls,Data)
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  return(list(Cls=Cls,Object=res))
}