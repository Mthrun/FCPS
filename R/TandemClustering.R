TandemClustering=function(Data,ClusterNo,Type="Reduced",PlotIt=FALSE,...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Number of clusters to search for
  #
  # OPTIONAL
  # Type              Reduced:   Reduced k-means (RKM) [De Soete/Carroll, 1994].
  #                   Factorial: Factorial k-mean (FKM) [Vichi/Kiers, 2001]
  #                   KernelPCA: Kernel PCA with minimum normalised cut hyperplanes [Hofmeyr/Pavlidis, 2019]
  # PlotIt            Boolean. Decision to plot or not
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of PPCI::ncuth algorithm
  #
  # Author: MT, 04/2020
  if (Type != 'KernelPCA') {
    d = dim(Data)[2]
    if (d < ClusterNo) {
      Cls1 = TandemClustering(Data,
                              ClusterNo = 2,
                              Type = Type,
                              PlotIt = FALSE,
                              ...)$Cls
      cc = length(unique(Cls1))
      while (cc < ClusterNo) {
        ind = which(Cls1 == 1)
        DataTMP = Data[ind, ]
        Cls1tmp = TandemClustering(
          DataTMP,
          ClusterNo = 2,
          Type = Type,
          PlotIt = FALSE,
          ...
        )$Cls
        NotInd = setdiff(1:nrow(Data), ind)
        Cls1[NotInd] = Cls1[NotInd] + 1
        Cls1[ind] = Cls1tmp
        Cls1 = ClusterRenameDescendingSize(Cls1)
        cc = length(unique(Cls1))
      }
      if (isTRUE(PlotIt)) {
        if (requireNamespace('DataVisualizations'))
          DataVisualizations::Plot3D(Data, Cls1)
        else
          warning('PlotIT unavailable because DataVisualizations not installed')
      }
      return(
        list(Cls = Cls1, Object = 'Recursively called, because number of dimensions was less than the number of variables.')
      )
    }
  } else{
    if (!missing(ClusterNo))
      message(
        'TandemClustering of type KernelPCA does not require "ClusterNo" and will determine the number of clusters automatically.'
      )
  }
  
  
  switch(
    Type,
    'Factorial' = {
      if (!requireNamespace('clustrd')) {
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
      out = clustrd::cluspca(data = Data,
                             nclus = ClusterNo,
                             method = 'FKM',
                             ...)
    },
    'Reduced' = {
      if (!requireNamespace('clustrd')) {
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
      
      out = clustrd::cluspca(data = Data,
                             nclus = ClusterNo,
                             method = 'RKM',
                             ...)
    },
    'KernelPCA' = {
      if (!requireNamespace('kernlab')) {
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
      if (!requireNamespace('PPCI')) {
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
      x2 = kernlab::kpca(Data, kernel = "rbfdot", kpar = list(sigma = 3))@rotated
      out = PPCI::ncuth(x2, ...)
    },
    {
      warning('Incorrect option selected')
      return('Incorrect option selected')
    }
  )
  #  out=out
  
  Cls = ClusterRenameDescendingSize(out$cluster)
  
  if (!is.null(rownames(Data)))
    names(Cls) = rownames(Data)
  else
    names(Cls) = 1:nrow(Data)
  
  if (isTRUE(PlotIt)) {
    ClusterPlotMDS(Data, Cls)
  }
  Cls = ClusterRename(Cls, Data)
  return(list(Cls = Cls, Object = out))
}