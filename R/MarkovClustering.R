MarkovClustering=function(Data=NULL,Adjacency=NULL,Radius=TRUE,addLoops = TRUE,PlotIt=FALSE,...){
  
  #author: MT, 04/2018
  if(!is.null(Data)){
      if(Radius==TRUE){
        Radius=AdaptGauss::ParetoRadius(Data)
      }
    if(Radius==FALSE){
      requireNamespace('ABCanalysis')
      x=DistanceMatrix(Data,outputisvector=T)
      x=x[lower.tri(x, diag = FALSE)]
      par=quantile(x,c(0.2013)) #geschaetzter paretorRadius
      xx=ABCanalysis::ABCRemoveSmallYields(x,0.5)
      x=xx$SubstantialData
      res=suppressWarnings(ABCanalysis::ABCanalysis(x))
      Radius=min(x[res$Aind])/max(x[res$Cind])
      
      
    }
    DistanceMatrix = as.matrix(dist(Data))
    AnzPunkte = nrow(DistanceMatrix)
    N = ncol(DistanceMatrix)
    Adjacency = matrix(0, ncol = N, nrow = N)
    for (i in 1:AnzPunkte) {
      RInd = which(DistanceMatrix[i, ] <= Radius, arr.ind = T)
      Adjacency[i, RInd] = 1
    }
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
    DataVisualizations::Plot3D(Data,Cls)
  }
  return(list(Cls=Cls,MCLobject=mm))
}