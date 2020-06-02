HCLclustering <-function(Data,ClusterNo,PlotIt=FALSE,...){
  # Cls=NeuralGas(Data,ClusterNo=2)
  #  
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Number of clusters to search for
  #
  # OPTIONAL
  # PlotIt            Boolean. Decision to plot or not
  # 
  # OUTPUT
  # Cls[1:n]     Clustering of data
  # Object       Object of cclust algorithm
  # 
  # Author: MT 04/2018


  if (!requireNamespace('cclust')) {
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
  
  res=cclust::cclust(x=Data,centers=ClusterNo,method='hardcl',...)
  Cls=res$cluster
  if(PlotIt){
   ClusterPlotMDS(Data,Cls)
  }
   Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=res))
}