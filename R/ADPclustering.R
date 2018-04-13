ADPclustering=function(Data,PlotIt=FALSE,...){
  
  #author: MT, 04/2018
  requireNamespace('ADPclust')
  adp=ADPclust::adpclust(Data,...)
  
  Cls=as.numeric(adp$clusters)
  
  if(PlotIt){
    requireNamespace('DataVisualizations')
    DataVisualizations::plot3D(Data,Cls)
  }
  return(list(Cls=Cls,ADPobject=adp))
}