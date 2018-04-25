MarkovClustering=function(Data=NULL,Adjacency=NULL,addLoops = TRUE,PlotIt=FALSE,...){
  
  #author: MT, 04/2018
  if(!is.null(Data)){
    requireNamespace('GraphAlgorithms')
    Adjacency=GraphAlgorithms::RkugelGraph(Data,R = AdaptGauss::ParetoRadius(Data))
  }  
  requireNamespace('MCL')
  
  mm=MCL::mcl(x = Adjacency,addLoops =addLoops,...)

  #Graph wahl noetig, daher automatisiert nicht nutzbar
  # Distance=DistanceMatrix(FCPS$Hepta$Data)
  # adjacency=KNNGraph(Distance,11)
  
  Cls=as.numeric(mm$Cluster)
  
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
  else
    names(Cls)=1:nrow(Data)
  
  if(PlotIt&!is.null(Data)){
    requireNamespace('DataVisualizations')
    DataVisualizations::plot3D(Data,Cls)
  }
  return(list(Cls=Cls,MCLobject=mm))
}