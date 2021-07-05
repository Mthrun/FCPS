HDDClustering=function(Data, ClusterNo, PlotIt=F,...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # 
  # OPTIONAL
  # ClusterNo         Numeric vector of integers specifying the number of
  #                   clusters for which the BIC and the parameters are to be
  #                   calculated; the function keeps the parameters which
  #                   maximises the BIC. Note that the length of the vector K
  #                   can't be larger than 20. Default is 1:10.
  # PlotIt            Boolean. Default = FALSE = No plotting performed.
  # ...               See HDclassif for more parameters.
  # 
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of hddc.
  #                   
  #                   
  # 
  # Author: QS, 06/2021
  if (!requireNamespace('HDclassif', quietly = TRUE)) {
    message(
      'Subordinate clustering package (HDclassif) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate clustering package (HDclassif) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  if(missing(Data)){
    message('Variable Data is not given. Returning.')
    return()
  }
  if(is.null(Data)){
    message('Variable Data is not given. Returning.')
    return()
  }
  if(!missing(ClusterNo))
    Object = HDclassif::hddc(data=Data, K=ClusterNo, ...)
  else
    Object = HDclassif::hddc(data=Data, ...)
  
  #Cls = apply(Object$posterior, 1, which.max)
  Cls=Object$class
  Cls=ClusterRename(Cls,Data)
  
  if(PlotIt == TRUE){
    FCPS::ClusterPlotMDS(Data, Cls, main = "Clustering",
                         DistanceMethod = "euclidean", OutputDimension = 3,
                         PointSize=1,Plotter3D="rgl", ...)
  }
  return(list("Cls"=Cls, "Object"=Object))
}