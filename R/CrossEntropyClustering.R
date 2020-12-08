CrossEntropyClustering=function(Data,ClusterNo,PlotIt=FALSE,...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Number of clusters to search for
  # 
  # OPTIONAL
  # PlotIt            Boolean. Decision to plot or not
  # 
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of CEC::cec algorithm
  #
  # Author: MT
  if (!requireNamespace('CEC',quietly = TRUE)) {
    message(
      'Subordinate clustering package (CEC) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate clustering package (CEC) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  model=CEC::cec(Data,centers = ClusterNo,...)
  
  Cls=model$cluster
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=model))
}