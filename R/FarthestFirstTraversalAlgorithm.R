FarthestFirstTraversalAlgorithm=function(Data,PlotIt=FALSE,...){
  requireNamespace('RWeka')
  
  out=RWeka::FarthestFirst(Data,...)
  
  Cls=out$class_ids
  
  
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
    Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=out))
}