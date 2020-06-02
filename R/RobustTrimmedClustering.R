RobustTrimmedClustering=function(Data,ClusterNo,Alpha,PlotIt=FALSE,...){
  # Cls=RobustTrimmedClustering(Data,ClusterNo)
  #
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Number of clusters to search for
  # Alpha             No trimming is done equals to alpha =0, otherwise proportion of datapoints to be trimmed.
  # 
  # OPTIONAL
  # PlotIt            Boolean. Decision to plot or not.
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of tclust::tclust
  #
  # Author: MT 09/2019  
  
  if (!requireNamespace('tclust')) {
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
  
  res=tclust::tclust(x=Data,k = ClusterNo,alpha = Alpha,...)
  Cls=res$cluster	
  
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=res))
}