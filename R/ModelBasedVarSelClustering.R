ModelBasedVarSelClustering=function(Data,ClusterNo,Type,PlotIt=FALSE, ...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Numeric. It defines number of components to consider.
  #
  # OPTIONAL
  # PlotIt            Boolean. Default = FALSE = No plotting performed.
  # ...               See VarSelLCM manual for more information
  # 
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of VSLCMresults-class.
  #                   
  # Author: QS, 06/2021
  if(missing(Data)){
    message('Variable Data is not given. Returning.')
    return()
  } 
  if(missing(ClusterNo)){
    message('Variable ClusterNo is not given. Returning.')
    return()
  } 
  
  if(is.null(Data)){
    message('Variable Data is not given. Returning.')
    return()
  }
  if(is.null(ClusterNo)){
    message('Variable ClusterNo is not given. Returning.')
    return()
  }
  if(missing(Type)){
    message('Variable Type is not given. Returning.')
    return()
  } 
  if(Type=="VarSelLCM"){
  if (!requireNamespace('VarSelLCM', quietly = TRUE)) {
    message(
      'Subordinate clustering package (VarSelLCM) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate clustering package (VarSelLCM) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }

  Object = VarSelLCM::VarSelCluster(x=Data, gvals=ClusterNo, ...)
  Cls = Object@partitions@zMAP
  if(length(Cls)<2){
    warning("ModelBasedVarSelClustering:: The subfunction VarSelLCM::VarSelCluster did not perfom any clustering, please contact the author for further information. Setting all clusters to 1")
    Cls=rep(1,nrow(Data))
  }
  }else if(Type=="clustvarsel"){
    if (!requireNamespace('clustvarsel', quietly = TRUE)) {
      message(
        'Subordinate clustering package (clustvarsel) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
      )
      return(
        list(
          Cls = rep(1, nrow(Data)),
          Object = "Subordinate clustering package (clustvarsel) is missing.
                Please install the package which is defined in 'Suggests'."
        )
      )
    }
    #make sure the algorithm finds in this namespace its subfunctions
    #to be improved later on
    clvarselgrfwd=clustvarsel::clvarselgrfwd
    clvarselgrbkw=clustvarsel::clvarselgrbkw
    clvarselhlfwd=clustvarsel::clvarselhlfwd
    clvarselhlbkw=clustvarsel::clvarselhlbkw
    Object=clustvarsel::clustvarsel(data = Data,G = ClusterNo,...)
    Cls=Object$model$classification
  }else{
    warning("Incorrect method selected.")
    return("Incorrect method selected.")
  }
  
  Cls=ClusterRename(Cls,Data)
  if(PlotIt == TRUE){
    FCPS::ClusterPlotMDS(Data, Cls, main = "Clustering",
                         DistanceMethod = "euclidean", OutputDimension = 3,
                         PointSize=1,Plotter3D="rgl", ...)
  }
  return(list("Cls"=Cls, "Object"=Object))
}