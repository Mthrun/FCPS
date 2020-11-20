Spectrum=function(Data,Type=2,ClusterNo=NULL,PlotIt=FALSE,Silent=TRUE,PlotResults=FALSE,...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # Type            Type = 1: Default eigengap method (Gaussian clusters)
  #                   Type = 2: multimodality gap method (Gaussian/ non-Gaussian clusters)
  #                   Type = 3: Allows to setClusterNo
  # ClusterNo         Number of clusters to search for
  #
  # OPTIONAL
  # PlotIt            Boolean. Decision to plot or not
  # Silent            Boolean. Show status messages.
  # PlotResults       Boolean. Visualize results or not.
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of Spectrum::Spectrum algorithm
  #
  # Author: MT
  if (!requireNamespace('Spectrum')) {
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
  if(is.null(ClusterNo))
    out=Spectrum::Spectrum(t(Data),method = Type,silent = Silent,showres =PlotResults ,...)
  else
    out=Spectrum::Spectrum(t(Data),fixk = ClusterNo,method = 3,ClusterNo,silent = Silent,showres =PlotResults,...)
  
  Cls=out$assignments
  
  
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=out))
}