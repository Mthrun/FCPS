FannyClustering=function(Data,ClusterNo,PlotIt=FALSE,...){
  
  #author: MT, 04/2018
  requireNamespace('cluster')
  fan=cluster::fanny(Data,k=ClusterNo,...)
  Cls=fan$clustering
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
  else
    names(Cls)=1:nrow(Data)
  
  Cls[!is.finite(Cls)]=0
  if(PlotIt){
    Cls2=Cls
    Cls2[Cls2==0]=999
    requireNamespace('DataVisualizations')
    DataVisualizations::plot3D(Data,Cls2)
  }
  return(list(Cls=Cls,ObjectFanny=fan))
}