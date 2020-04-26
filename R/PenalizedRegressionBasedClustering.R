PenalizedRegressionBasedClustering=function(Data,FirstLambda,SecondLambda,Tau, PlotIt=FALSE,...){
  
  Data=t(Data)
  model=prclust::PRclust(Data,FirstLambda,SecondLambda,Tau)
  
  Cls=model$group
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=model))
}