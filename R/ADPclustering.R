ADPclustering=function(Data,ClusterNo=NULL,PlotIt=FALSE,...){
  
  #author: MT, 04/2018
  requireNamespace('ADPclust')
  if(is.null(ClusterNo))
	adp=ADPclust::adpclust(Data,...)
  else
  adp=ADPclust::adpclust(Data,nclust=ClusterNo,...)
  
  Cls=as.numeric(adp$clusters)
  
  if(PlotIt){
    requireNamespace('DataVisualizations')
    DataVisualizations::plot3D(Data,Cls)
  }
  return(list(Cls=Cls,ADPobject=adp))
}