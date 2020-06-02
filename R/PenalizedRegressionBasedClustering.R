PenalizedRegressionBasedClustering=function(Data,FirstLambda,SecondLambda,Tau, PlotIt=FALSE,...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # FirstLambda       Set 1 for quadratic penalty based algorithm, 0.4 for revised ADMM.
  # SecondLambda      The magnitude of grouping penalty.
  # Tau               Tuning parameter: tau, related to grouping penalty.
  # 
  # OPTIONAL
  # PlotIt            Boolean. Decision to plot or not
  # 
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of prclust::PRclust algorithm
  #
  # Author: MT
  if (!requireNamespace('prclust')) {
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
  
  Data=t(Data)
  model=prclust::PRclust(Data,FirstLambda,SecondLambda,Tau)
  
  Cls=model$group
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=model))
}