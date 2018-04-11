FannyClustering=function(Data,k,PlotIt=FALSE,...){
  
  #author: MT, 04/2018
  requireNamespace('cluster')
  fan=cluster::fanny(Data,k=k,...)
  Cls=fan$clustering
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
  else
    names(Cls)=1:nrow(Data)
  
  if(PlotIt){
    requireNamespace('DataVisualizations')
    DataVisualizations::plot3D(Data,Cls)
  }
  return(list(Cls=Cls,ObjectFanny=fan))
}