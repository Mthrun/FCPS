RandomForestClustering=function(Data,ClusterNo,Type="ward.D2",NoTrees = 2000,PlotIt=FALSE,PlotForest=FALSE,...){
  if (!requireNamespace('randomForest')) {
    message(
      'Subordinate package (randomForest) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate package (randomForest) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  if (!requireNamespace('cluster',quietly = TRUE)) {
    message(
      'Subordinate clustering package (cluster) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate clustering package (cluster) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  rf=randomForest::randomForest(x = Data, ntree = NoTrees, proximity = TRUE,type="unsupervised",...)
  proximity=rf$proximity
  Dissimilarity=max(proximity)-proximity #sqrt(1-proximity)
  
  if(Type=="PAM"){
    CA=cluster::pam(x=Dissimilarity,diss = T,k=ClusterNo)
    Cls=CA$clustering
  }else{
    CA=HierarchicalClusterDists(Dissimilarity,ClusterNo=ClusterNo,method=Type,Fast=FALSE)
    Cls=CA$Cls
  }
  
  Cls = ClusterRename(Cls, Data)
  
  if (PlotIt) {
    ClusterPlotMDS(Data, Cls)
  }
  if (PlotForest) {
    randomForest::MDSplot(rf, as.factor(Cls))
  }
  return(list(
    Cls = Cls,
    Object = list(rf=rf,CA=CA)
  ))
}
  
  