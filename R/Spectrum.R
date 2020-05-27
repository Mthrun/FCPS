Spectrum=function(Data,Method=2,ClusterNo=NULL,PlotIt=FALSE,Silent=TRUE,PlotResults=FALSE,...){
  
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
    out=Spectrum::Spectrum(t(Data),method = Method,silent = Silent,showres =PlotResults ,...)
  else
    out=Spectrum::Spectrum(t(Data),fixk = ClusterNo,method = 3,ClusterNo,silent = Silent,showres =PlotResults,...)
  
  Cls=out$assignments
  
  
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=out))
}