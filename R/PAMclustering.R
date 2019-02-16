PAMClustering=function(Data,ClusterNo,PlotIt=FALSE,...){
  
  #author: MT, 04/2018
  requireNamespace('cluster')
  pam=cluster::pam(x=Data,k=ClusterNo,...)
  Cls=pam$clustering
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
  else
    names(Cls)=1:nrow(Data)
  
  if(PlotIt){
    requireNamespace('DataVisualizations')
    DataVisualizations::Plot3D(Data,Cls)
  }
  return(list(Cls=Cls,ObjectPAM=pam))
}