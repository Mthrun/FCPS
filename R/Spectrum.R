Spectrum=function(Data,Method=2,ClusterNo=NULL,PlotIt=FALSE,Silent=TRUE,PlotResults=FALSE,...){
  requireNamespace('Spectrum')
  
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