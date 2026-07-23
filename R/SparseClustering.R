SparseClustering=function(DataOrDistances, ClusterNo, Type="Hierarchical",PlotIt=F,
                          Silent=FALSE, NoPerms=10,Wbounds, ...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Numeric indicating number to cluster to find in Tree/
  #                   Dendrogramm in case of Type="Hierarchical" and to
  #                   construct in case of Type="KMeans"
  # 
  # OPTIONAL
  # ...               See more about parameters in mvnormalmixEM
  # Type          Char selecting methods Hierarchical or k-means
  #                   Default: "Hierarchical"
  # PlotIt            Boolean. Default = FALSE = No plotting performed.
  # Silent            Boolean: print output or not (Default = FALSE = no output)
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of VSLCMresults-class.
  # Tree              Object Tree if Type="Hierachical" is used
  # 
  # Author: QS, 06/2021
  if (!requireNamespace('sparcl', quietly = TRUE)) {
    message(
      'SparseClustering: Subordinate clustering package (sparcl) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(DataOrDistances)),
        Object = "SparseClustering: Subordinate clustering package (sparcl) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  if(missing(DataOrDistances)){
    # if(!missing(Data))
    #   DataOrDistances=Data ##for parApplyDataBasedCA
    # else{
      message('SparseClustering: Variable Data is not given. Returning.')
      return()
    # }
  }
  if(is.null(DataOrDistances)){
    message('SparseClustering: Variable Data is not given. Returning.')
    return()
  }
  if(missing(Wbounds)){
    Wbounds=NULL
  }
  
  if (isSymmetric(unname(DataOrDistances))) {
    message('SparseClustering: For symmetric "DataOrDistances" distances are assumed and Type is automatically set to "Hierarchical"
            because for Type="kmeans" the usage of distances is not preferable.')
    Type="Hierarchical"
  }
  if(Type=="Hierarchical"){
    # N = dim(Data)[1]
    # D = dim(Data)[2]
    if (isSymmetric(unname(DataOrDistances))) {
      V      = sparcl::HierarchicalSparseCluster(dists=DataOrDistances, silent=Silent,wbound = Wbounds,...)
    }else{
      perm.out = sparcl::HierarchicalSparseCluster.permute(DataOrDistances,,wbounds = Wbounds,nperms = NoPerms)
      dists  = perm.out$dists
      wbound = perm.out$bestw
      V      = sparcl::HierarchicalSparseCluster(x=NULL, dists=dists,
                                                 wbound=wbound, silent=Silent,...)
    }
    Tree = V$hc
    Cls  = as.vector(cutree(Tree, ClusterNo))
    Cls=ClusterRename(Cls,DataOrDistances)
    if(PlotIt == TRUE){
      ClusterDendrogram(Tree, ClusterNo, main='Hierarchical sparse clustering')
    }
    return(list("Cls"=Cls, "Object"=V, "Dendrogram"=Tree))
  }else{
    km.perm = sparcl::KMeansSparseCluster.permute(DataOrDistances, K=ClusterNo, silent=Silent,nperms = NoPerms,wbounds = Wbounds)
    km.out  = sparcl::KMeansSparseCluster(DataOrDistances, K=ClusterNo,wbounds = km.perm$bestw, silent=Silent, ...)
    Cls     = as.vector(km.out[[1]]$Cs)
    Cls=ClusterRename(Cls,DataOrDistances)
    if(PlotIt == TRUE){
      FCPS::ClusterPlotMDS(DataOrDistances, Cls, main = "k-means sparse clustering",
                           DistanceMethod = "euclidean", OutputDimension = 3,
                           PointSize=1,Plotter3D="rgl", ...)
    }
    return(list("Cls"=Cls, "Object"=km.out))
  }
}