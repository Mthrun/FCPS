COBWEB=function(Data,PlotIt=FALSE,...){
  requireNamespace('RWeka')
  
  out=RWeka::Cobweb(Data,...)
  
  Cls=out$class_ids
  
  
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
    Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=out))
}