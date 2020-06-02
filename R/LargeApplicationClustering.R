LargeApplicationClustering <-function(Data,ClusterNo,PlotIt=FALSE,Standardization=TRUE,Samples=50,Random=TRUE,...){
  # Cls=LargeApplicationClustering(Data,ClusterNo=2)
  # Clustering Large Applications  (clara)
  #
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Number of clusters to search for
  # 
  # OPTIONAL
  # PlotIt            Boolean. Decision to plot or not
  # Standardization   Data is standardized before calculating the dissimilarities.
  #                   Measurements are standardized for each variable (column), by subtracting the
  #                   variable's mean value and dividing by the variable's mean absolute deviation.
  # Samples           integer, say N, the number of samples to be drawn from the dataset. Default value
  #                   set as recommended by documentation of cluster::clara.
  # Random            logical indicating if R's random number generator should be used instead of the primitive clara()-builtin one.
  #
  # OUTPUT
  # Cls[1:n]    Clustering of data
  # Object      Object of cluster::clara algorithm
  #
  # Author: MT 04/2018
  
  if (!requireNamespace('cluster')) {
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
  
  if(Standardization==1) Standardization=TRUE
  if(Standardization==0) Standardization=FALSE
  
  res=cluster::clara(x=Data,k = ClusterNo,samples=Samples,rngR=Random,stand=Standardization,...)
  Cls=res$clustering

  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  	Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=res))
}