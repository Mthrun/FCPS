kmeansClustering <-function(DataOrDistances,ClusterNo=2,Type='LBG',RandomNo=5000,CategoricalData,PlotIt=FALSE,Verbose=FALSE,...){
  # Cls <- kmeansClustering(DataOrDistances,ClusterNo,Verbose);
  # calls one of two common approaches for kmeans
  #
  # INPUT
  # DataOrDistancess[1:n,1:d]    Dataset with n observations and d features or distance matrix with size n
  # ClusterNo                   Number of clusters to search for
  #
  # Type       Kind of kmeans algorithm. Choose one of the two following strings:
  #            "Hartigan": Hartigan, J. A. and Wong, M. A. A K-means clustering algorithm. Applied Statistics 28, 100-108, 1979.
  #            "LBG": Linde,Y.,Buzo,A.,Gray,R.M., An algorithm for vector quantizer design. IEEE Transactions on Communications, COM-28, 84-95, 1980
  #             ’pelleg-moore’, ’elkan’, ’hamerly’,’dualtree’, or ’dualtree-covertree’
  # RandomNo   Only for Steinley method or in case of distance matrix, number of random initializations with
  #            searching for minimal SSE, see [Steinley/Brusco, 2007]
  # PlotIt     Boolean. Decision tgo plot or not
  # Verbose    '0' or '1' for a documentation of repetitions. Default: Verbose==0
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Contains two variables: Centroids and SumDistsToCentroids.
  #
  # ALU 2014, MT 2016
  # Adaption to Mdbt and documentation standards
  if (!isSymmetric(unname(DataOrDistances))) {
    #Data = DataOrDistances
    if(missing(CategoricalData)&Type=="kprototypes"){
      warning("kmeansClustering: CategoricalData cannot be missing if Type is 'kprototypes'. Setting type to default")
      Type="Hartigan"
    }
    if (ClusterNo < 2) {
      warning("ClusterNo should be an integer > 2. Now, all of your data is in one cluster.")
      if (is.null(nrow(DataOrDistances))) {
        # then we got a vector
        return(cls <- rep(1, length(DataOrDistances)))
      } else{
        # Matrix
        return(cls <- rep(1, nrow(DataOrDistances)))
      }
    }
    
    switch(
      Type,
      'Hartigan' = {
        res = kmeans(DataOrDistances, centers = ClusterNo,algorithm = "Hartigan-Wong", ...)
        Cls = as.vector(res$cluster)
        
        if (Verbose == TRUE) {
          print(res)
        }
        if (PlotIt) {
          ClusterPlotMDS(DataOrDistances, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistances)
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
              Cls = rep(1, nrow(DataOrDistances)),
              Object = "Subordinate clustering (flexclust) package is missing.
                Please install the package which is defined in 'Suggests'."
            )
          )
        }
        res = flexclust::kcca(x = DataOrDistances, k = ClusterNo, ...)
        
        Cls = as.vector(res@cluster)
        
        Centroids=res@centers
        if (PlotIt) {
          ClusterPlotMDS(DataOrDistances, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistances)
        return(list(
          Cls = Cls,
          Object = res,
          Centroids = Centroids
        ))
      },'kprototypes' = {
        if (!requireNamespace('clustMixType',quietly = TRUE)) {
          message(
            'Subordinate clustering (clustMixType) package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
          )
          return(
            list(
              Cls = rep(1, nrow(DataOrDistances)),
              Object = "Subordinate clustering (clustMixType) package is missing.
                Please install the package which is defined in 'Suggests'."
            )
          )
        }
        DataOrDistancesWithFactors=as.data.frame(DataOrDistances)
        CategoricalData=as.data.frame(CategoricalData)
        for(i in 1:ncol(CategoricalData)){
          CategoricalData[,i]=as.factor(CategoricalData[,i])
        }
        DataOrDistancesWithFactors=cbind(DataOrDistancesWithFactors,CategoricalData)
        
        res = clustMixType::kproto(x = DataOrDistancesWithFactors, k = ClusterNo, ...)#verbose=FALSE,
        
        Cls = as.numeric((res$cluster))
        
        Centroids=res$centers
        if (PlotIt) {
          ClusterPlotMDS(DataOrDistances, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistances)
        return(list(
          Cls = Cls,
          Object = res,
          Centroids = Centroids
        ))
      },
      'LBG' = {
        if (!requireNamespace('cclust',quietly = TRUE)) {
          message(
            'Subordinate clustering package (cclust) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
          )
          return(
            list(
              Cls = rep(1, nrow(DataOrDistances)),
              Object = "Subordinate clustering package (cclust) is missing.
                Please install the package which is defined in 'Suggests'."
            )
          )
        }
        
        res = cclust::cclust(
          x = DataOrDistances,
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
          ClusterPlotMDS(DataOrDistances, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistances)
        return(list(
          Cls = Cls,
          Object = res,
          Centroids = res$centers
        ))
      },
      "Sparse" = {
        out=SparseClustering(DataOrDistances = DataOrDistances,
                             ClusterNo = ClusterNo,Strategy = 'kmeans',...)
        return(list(
          Cls = out$Cls,
          Object = out$Object
        ))
      },
      "Steinley" = {
        Liste = lapply(1:RandomNo, function(i, DataOrDistances, centers, ...) {
          c = kmeans(DataOrDistances, centers = ClusterNo, ...)
          return(list(sum(c$withinss), kmeansOut = c))
        }, DataOrDistances, ClusterNo, ...)
        SSEs = unlist(lapply(Liste, "[[", 1))
        res = Liste[[which.min(SSEs)]]$kmeansOut
        Cls = as.vector(res$cluster)
        if (PlotIt) {
          requireNamespace('DataVisualizations',quietly = TRUE)
          ClusterPlotMDS(DataOrDistances, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistances)
        return(list(
          Cls = Cls,
          Object = list(
            res
          ),
          Centroids = res$centers
        ))
      },"Pelleg-moore"={
        if (!requireNamespace('mlpack',quietly = TRUE)) {
          message(
            'Subordinate clustering package (mlpack) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
          )
          return(
            list(
              Cls = rep(1, nrow(DataOrDistances)),
              Object = "Subordinate clustering package (mlpack) is missing.
                Please install the package which is defined in 'Suggests'."
            )
          )
        }
        res = mlpack::kmeans(input = DataOrDistances, clusters = ClusterNo, algorithm = tolower(Type),labels_only = T, ...)
        Cls = as.vector(res$output)+1
        if (PlotIt) {
          ClusterPlotMDS(DataOrDistances, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistances)
        return(list(
          Cls = Cls,
          Object = res,
          Centroids = res$centroid
        ))
      },"Elkan"={
        if (!requireNamespace('mlpack',quietly = TRUE)) {
          message(
            'Subordinate clustering package (mlpack) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
          )
          return(
            list(
              Cls = rep(1, nrow(DataOrDistances)),
              Object = "Subordinate clustering package (mlpack) is missing.
                Please install the package which is defined in 'Suggests'."
            )
          )
        }
        res = mlpack::kmeans(input = DataOrDistances, clusters = ClusterNo, algorithm = tolower(Type),labels_only = T, ...)
        Cls = as.vector(res$output)+1
        if (PlotIt) {
          ClusterPlotMDS(DataOrDistances, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistances)
        return(list(
          Cls = Cls,
          Object = res,
          Centroids = res$centroid
        ))
      },"Hamerly"={
        if (!requireNamespace('mlpack',quietly = TRUE)) {
          message(
            'Subordinate clustering package (mlpack) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
          )
          return(
            list(
              Cls = rep(1, nrow(DataOrDistances)),
              Object = "Subordinate clustering package (mlpack) is missing.
                Please install the package which is defined in 'Suggests'."
            )
          )
        }
        res = mlpack::kmeans(input = DataOrDistances, clusters = ClusterNo, algorithm = tolower(Type),labels_only = T, ...)
        Cls = as.vector(res$output)+1
        if (PlotIt) {
          ClusterPlotMDS(DataOrDistances, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistances)
        return(list(
          Cls = Cls,
          Object = res,
          Centroids = res$centroid
        ))
      },"Dualtree"={
        if (!requireNamespace('mlpack',quietly = TRUE)) {
          message(
            'Subordinate clustering package (mlpack) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
          )
          return(
            list(
              Cls = rep(1, nrow(DataOrDistances)),
              Object = "Subordinate clustering package (mlpack) is missing.
                Please install the package which is defined in 'Suggests'."
            )
          )
        }
        res = mlpack::kmeans(input = DataOrDistances, clusters = ClusterNo, algorithm = tolower(Type),labels_only = T, ...)
        Cls = as.vector(res$output)+1
        if (PlotIt) {
          ClusterPlotMDS(DataOrDistances, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistances)
        return(list(
          Cls = Cls,
          Object = res,
          Centroids = res$centroid
        ))
      },"Dualtree-covertree"={
        if (!requireNamespace('mlpack',quietly = TRUE)) {
          message(
            'Subordinate clustering package (mlpack) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
          )
          return(
            list(
              Cls = rep(1, nrow(DataOrDistances)),
              Object = "Subordinate clustering package (mlpack) is missing.
                Please install the package which is defined in 'Suggests'."
            )
          )
        }
        res = mlpack::kmeans(input = DataOrDistances, clusters = ClusterNo, algorithm = tolower(Type),labels_only = T, ...)
        Cls = as.vector(res$output)+1
        if (PlotIt) {
          ClusterPlotMDS(DataOrDistances, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistances)
        return(list(
          Cls = Cls,
          Object = res,
          Centroids = res$centroid
        ))
      },
      {#lloyd, forgy, mac queen
        res = kmeans(DataOrDistances, centers = ClusterNo, algorithm = Type, ...)
        Cls = as.vector(res$cluster)
        
        if (Verbose == TRUE) {
          print(res)
        }
        if (PlotIt) {
          ClusterPlotMDS(DataOrDistances, Cls)
        }
        Cls = ClusterRename(Cls, DataOrDistances)
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
        Distance = DataOrDistances,
        ClusterNo = ClusterNo,
        RandomNo = RandomNo,
        PlotIt = PlotIt,
        verbose = Verbose,
        ...
      )
    )
  }
}
