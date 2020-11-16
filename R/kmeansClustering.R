kmeansClustering <-function(DataOrDistance,ClusterNo=2,Type='LBG',RandomNo=5000,PlotIt=FALSE,Verbose=FALSE,...){
  # Cls <- kmeansClustering(DataOrDistance,ClusterNo,Verbose);
  # calls one of two common approaches for kmeans
  #
  # INPUT
  # DataOrDistances[1:n,1:d]    Dataset with n observations and d features or distance matrix with size n
  # ClusterNo                   Number of clusters to search for
  #
  # Type       Kind of kmeans algorithm. Choose one of the two following strings:
  #            "Hartigan": Hartigan, J. A. and Wong, M. A. A K-means clustering algorithm. Applied Statistics 28, 100-108, 1979.
  #            "LBG": Linde,Y.,Buzo,A.,Gray,R.M., An algorithm for vector quantizer design. IEEE Transactions on Communications, COM-28, 84-95, 1980
  # RandomNo   Only for Steinley method or in case of distance matrix, number of random initializations with
  #            searching for minimal SSE, see [Steinley/Brusco, 2007]
  # PlotIt     Boolean. Decision to plot or not
  # Verbose    '0' or '1' for a documentation of repetitions. Default: Verbose==0
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Contains two variables: Centroids and SumDistsToCentroids.
  #
  # ALU 2014, MT 2016
  # Adaption to Mdbt and documentation standards
  if (!isSymmetric(unname(DataOrDistance))) {
    #Data = DataOrDistance
    
    if (ClusterNo < 2) {
      warning("ClusterNo should be an integer > 2. Now, all of your data is in one cluster.")
      if (is.null(nrow(DataOrDistance))) {
        # then we got a vector
        return(cls <- rep(1, length(DataOrDistance)))
      } else{
        # Matrix
        return(cls <- rep(1, nrow(DataOrDistance)))
      }
    }
    
    switch(
      Type,
      'Hartigan' = {
        res = kmeans(DataOrDistance, centers = ClusterNo,algorithm = "Hartigan-Wong", ...)
        Cls = as.vector(res$cluster)
        
        if (Verbose == TRUE) {
          print(res)
        }
        if (PlotIt) {
          ClusterPlotMDS(DataOrDistance, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistance)
        return(list(
          Cls = Cls,
          Object = list(
            res
          ),
          Centroids = res$centers
        ))
      },
      'kcentroids' = {
        if (!requireNamespace('flexclust',quietly = TRUE)) {
          message(
            'Subordinate clustering (flexclust) package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
          )
          return(
            list(
              Cls = rep(1, nrow(DataOrDistance)),
              Object = "Subordinate clustering (flexclust) package is missing.
                Please install the package which is defined in 'Suggests'."
            )
          )
        }
        res = flexclust::kcca(x = DataOrDistance, k = ClusterNo, ...)
        
        Cls = as.vector(res@cluster)
        
        Centroids=res@centers
        if (PlotIt) {
          ClusterPlotMDS(DataOrDistance, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistance)
        return(list(
          Cls = Cls,
          Object = res,
          Centroids = Centroids
        ))
      },
      'LBG' = {
        if (!requireNamespace('cclust',quietly = TRUE)) {
          message(
            'Subordinate clustering package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
          )
          return(
            list(
              Cls = rep(1, nrow(DataOrDistance)),
              Object = "Subordinate clustering package is missing.
                Please install the package which is defined in 'Suggests'."
            )
          )
        }
        
        res = cclust::cclust(
          x = DataOrDistance,
          centers = ClusterNo,
          method = 'kmeans',
          verbose = Verbose,
          ...
        )
        Cls = res$cluster
        SSE = res$withinss
        # s. http://epub.wu.ac.at/1542/1/document.pdf p.5
        # An examination of indices for determining the number of clusters in binary data sets
        # Weingessel, Andreas and Dimitriadou, Evgenia and Dolnicar, Sara (1999)
        if (PlotIt) {
          ClusterPlotMDS(DataOrDistance, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistance)
        return(list(
          Cls = Cls,
          Object = res,
          Centroids = res$centers
        ))
      },
      "Steinley" = {
        Liste = lapply(1:RandomNo, function(i, DataOrDistance, centers, ...) {
          c = kmeans(DataOrDistance, centers = ClusterNo, ...)
          return(list(sum(c$withinss), kmeansOut = c))
        }, DataOrDistance, ClusterNo, ...)
        SSEs = unlist(lapply(Liste, "[[", 1))
        res = Liste[[which.min(SSEs)]]$kmeansOut
        Cls = as.vector(res$cluster)
        if (PlotIt) {
          requireNamespace('DataVisualizations',quietly = TRUE)
          ClusterPlotMDS(DataOrDistance, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistance)
        return(list(
          Cls = Cls,
          Object = list(
            SumDistsToCentroids = res$withinss,
          ),
          Centroids = res$centers
        ))
      },
      {#lloyd, forgy, mac queen
        res = kmeans(DataOrDistance, centers = ClusterNo, algorithm = Type, ...)
        Cls = as.vector(res$cluster)
        
        if (Verbose == TRUE) {
          print(res)
        }
        if (PlotIt) {
          ClusterPlotMDS(DataOrDistance, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistance)
        return(list(
          Cls = Cls,
          Object = res,
          Centroids = res$centers
        ))
      }
    )
  } else{
    message(
      'Currently, the "Type" parameter of k-means cannot be set in the case of using a distance matrix.'
    )
    return(
      kmeansDist(
        Distance = DataOrDistance,
        ClusterNo = ClusterNo,
        RandomNo = RandomNo,
        PlotIt = PlotIt,
        verbose = Verbose,
        ...
      )
    )
  }
}
