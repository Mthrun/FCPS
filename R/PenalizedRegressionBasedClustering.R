PenalizedRegressionBasedClustering=function(Data,FirstLambda,SecondLambda,Tau, PlotIt=FALSE,...){
  
  
  if (!requireNamespace('prclust')) {
    message(
      'Subordinate clustering package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate clustering package is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  
  Data=t(Data)
  model=prclust::PRclust(Data,FirstLambda,SecondLambda,Tau)
  
  Cls=model$group
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=model))
}