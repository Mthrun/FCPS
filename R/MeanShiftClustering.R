MeanShiftClustering=function(Data,PlotIt=FALSE,...){
  # Cls=MeanShiftClustering(Data,ClusterNo=2)
  # Clustering by mean shift
  #
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # 
  # OPTIONAL
  # PlotIt            Boolean. Decision to plot or not
  #
  # OUTPUT
  # Cls[1:n]    Clustering of data
  # Object      Object of mlpack::mean_shift algorithm
  #
  # Author: MT 05/2023
  #Cheng, Yizong ( 1995). "Mean Shift, Mode Seeking, and Clustering". IEEE Transactions on Pattern Analysis and Machine Intelligence. 17 (8): 790–799. CiteSeerX 10.1.1.510.1222. doi:10.1109/34.400568.
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
  res = mlpack::mean_shift(input = Data,labels_only = T, ...)
  Cls = as.vector(res$output)+1

  if (PlotIt) {
    ClusterPlotMDS(Data , Cls)
  }
  Cls = ClusterRename(Cls, Data)
  
  return(list(Cls=Cls,Object=res))
}