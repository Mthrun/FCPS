kmeansClustering <-function(DataOrDistance,ClusterNo=2,Centers=NULL,Type='LBG',RandomNo=5000,PlotIt=FALSE,Verbose=FALSE,...){
# Cls <- kmeansClustering(Data,ClusterNo,Verbose);
# calls one of two common approaches for kmeans
# INPUT
# Data[1:n]                  der Datensatz in Zeilenvektoren 
# ClusterNo  in soviele Cluster werden die daten eingeteilt
#
# OPTIONAL
# Centers                default(NULL), a set of initial (distinct) cluster centres
# Type                 kind of kmeans algorithm, choose one of following strings
#                       "Hartigan": Hartigan, J. A. and Wong, M. A. A K-means clustering algorithm. Applied Statistics 28, 100-108, 1979.
#                       "LBG": Linde,Y.,Buzo,A.,Gray,R.M., An algorithm for vector quantizer design. IEEE Transactions on Communications, COM-28, 84-95, 1980
# Verbose               '0' or '1' for a documentation of repetitions, default: Verbose==0
#
#
# OUTPUT List V with
# Cls[1:n]                k-means Clusterung der Daten
# SumDistsToCentroids     Vector of within-cluster sum of squares, one component per cluster
# Centroids               The final cluster centers.
# ALU 2014, MT 2016
# angepasst an Mdbt und Doku standards
  if (!isSymmetric(unname(DataOrDistance))) {
    Data = DataOrDistance
    
    if (ClusterNo < 2) {
      warning("ClusterNo should to be an integer > 2. Now, all of your data is in one cluster.")
      if (is.null(nrow(Data))) {
        # dann haben wir einen Vektor
        return(cls <- rep(1, length(Data)))
      } else{
        # Matrix
        return(cls <- rep(1, nrow(Data)))
      }
    }
    
    if (!is.null(Centers))
      ClusterNo = Centers  # fuer Backward Compatibility
  
    
    switch(
      Type,
      'Hartigan' = {
        res = kmeans(Data, centers = ClusterNo, ...)
        Cls = as.vector(res$cluster)
        
        if (Verbose == TRUE) {
          print(res)
        }
        if (PlotIt) {
          ClusterPlotMDS(Data, Cls)
        }
        Cls = ClusterRename(Cls, Data)
        return(list(
          Cls = Cls,
          Object = list(
            SumDistsToCentroids = res$withinss,
            Centroids = res$centers
          )
        ))
      },
      'LBG' = {
        if (!requireNamespace('cclust')) {
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
        
        res = cclust::cclust(
          x = Data,
          centers = ClusterNo,
          method = 'kmeans',
          verbose = Verbose,
          ...
        )
        Cls = res$cluster
        SSE = res$withinss
        # s. http://epub.wu.ac.at/1542/1/document.pdf p.5
        # An examination of indexes for determining the number of clusters in binary data sets
        # Weingessel, Andreas and Dimitriadou, Evgenia and Dolnicar, Sara (1999)
        if (PlotIt) {
          ClusterPlotMDS(Data, Cls)
        }
        Cls = ClusterRename(Cls, Data)
        return(list(
          Cls = Cls,
          Object = list(
            SumDistsToCentroids = SSE,
            Centroids = res$centers
          )
        ))
      },
      "Steinley" = {
        Liste = lapply(1:RandomNo, function(i, Data, centers, ...) {
          c = kmeans(Data, centers = ClusterNo, ...)
          return(list(sum(c$withinss), kmeansOut = c))
        }, Data, ClusterNo, ...)
        SSEs = unlist(lapply(Liste, "[[", 1))
        res = Liste[[which.min(SSEs)]]$kmeansOut
        Cls = as.vector(res$cluster)
        if (PlotIt) {
          requireNamespace('DataVisualizations')
          ClusterPlotMDS(Data, Cls)
        }
        Cls = ClusterRename(Cls, Data)
        return(list(
          Cls = Cls,
          Object = list(
            SumDistsToCentroids = res$withinss,
            Centroids = res$centers
          )
        ))
      },
      {#lloyd, forgy, mac queen
        res = kmeans(Data, centers = ClusterNo, algorithm = Type, ...)
        Cls = as.vector(res$cluster)
        
        if (Verbose == TRUE) {
          print(res)
        }
        if (PlotIt) {
          ClusterPlotMDS(Data, Cls)
        }
        Cls = ClusterRename(Cls, Data)
        return(list(
          Cls = Cls,
          Object = list(
            SumDistsToCentroids = res$withinss,
            Centroids = res$centers
          )
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
        Centers = Centers,
        RandomNo = RandomNo,
        PlotIt = PlotIt,
        verbose = Verbose,
        ...
      )
    )
  }
}
