SubspaceClustering <-function(Data,ClusterNo,DimSubspace,method='Orclus',PlotIt=FALSE,OrclusInitialClustersNo=ClusterNo+2,...){
  # Cls=SubspaceClustering(Data,ClusterNo=2)
  #  
  # liefert eine Klassenzuweisung
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Number of clusters to search for
  #
  # OPTIONAL
  # PlotIt            Boolean. Decision to plot or not
  # method            'Orclus', Subspace Clustering Based on Arbitrarily Oriented Projected Cluster Generation
  #                   'ProClus'
  #                   'SubClu'
  #                   'Clique'
  #
  # OrclusInitialClustersNo
  # note: JAVA_HOME has to be set for rJava to use this method
  # 
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of subspace::CLIQUE
  # Method        
  # 
  # Author: MT 04/2018
  
  # Orclus
  d = dim(Data)[2]
  n = d = dim(Data)[1]
  
  switch(
    method,
    Orclus = {
      if (missing(DimSubspace)) {
        if (d > 3) {
          DimSubspace = dim(Data)[2] - 1
          if (DimSubspace > n / ClusterNo) {
            DimSubspace = n / ClusterNo - 1
          }
          DimSubspace = min(c(DimSubspace, 20)) # Higher subspace is not computable
        } else{
          DimSubspace = dim(Data)[2] * 0.99
        }
      }
      
      
      if (!requireNamespace('orclus')) {
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
      obj = orclus::orclus(x = Data,
                           k = ClusterNo,
                           l = DimSubspace,
                           k0 = OrclusInitialClustersNo,
                           ...)
      Cls = obj$cluster
    },
    ProClus = {
      if (!requireNamespace('subspace')) {
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
      
      if (!missing(DimSubspace))
        obj = subspace::ProClus(data = Data, k = ClusterNo, d = DimSubspace, ...)
      else
        obj = subspace::ProClus(data = Data, k = ClusterNo, ...)
      
      Cls = rep(NaN, nrow(Data))
      for (i in 1:length(obj)) {
        Cls[obj[[i]]$objects] = i
      }
      Cls[!is.finite(Cls)] = 9999
    },
    SubClu = {
      if (!requireNamespace('subspace')) {
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
      
      obj = subspace::SubClu(data = Data, ...)
      
      Cls = rep(NaN, nrow(Data))
      for (i in 1:length(obj)) {
        Cls[obj[[i]]$objects] = i
      }
      Cls[!is.finite(Cls)] = 9999
    },
    Clique = {
      if (!requireNamespace('subspace')) {
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
      obj = subspace::CLIQUE(data = Data, ...)
      
      Cls = rep(NaN, nrow(Data))
      for (i in 1:length(obj)) {
        Cls[obj[[i]]$objects] = i
      }
      Cls[!is.finite(Cls)] = 9999
    },
    stop("Wrong method string entered")
    
  )
  
  if (PlotIt) {
    ClusterPlotMDS(Data, Cls)
  }
  Cls = ClusterRename(Cls, Data)
  return(list(
    Cls = Cls,
    Object = obj,
    Method = method
  ))
}