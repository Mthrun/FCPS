ModelBasedClustering <-function(Data,ClusterNo=2,PlotIt=FALSE,...){
  # Cls <- MoGclustering(Data,ClusterNo);
  # call R's Model based clustering or MixtureOfGaussians (MoG) clustering
  #
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Number of clusters to search for
  #
  # OPTIONAL
  # PlotIt            Boolean. Decision to plot or not
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of mclust::Mclust algorithm
  #
  # MT 2017
  # Uebersicht/Kurz-Zfssg in  [Thrun, 2017, p. 23]
  #
  # [Thrun, 2017]  Thrun, M. C.:A System for Projection Based Clustering through Self-Organization and Swarm Intelligence, (Doctoral dissertation), Philipps-Universitaet Marburg, Marburg, 2017.
  # Algorithmus from:  
  # [Fraley/Raftery, 2002]  Fraley, C., & Raftery, A. E.: Model-based clustering, discriminant analysis, and density estimation, Journal of the American Statistical Association, Vol. 97(458), pp. 611-631. 2002.
  # [Fraley/Raftery, 2006]  Fraley, C., & Raftery, A. E.MCLUST version 3: an R package for normal mixture modeling and model-based clustering,DTIC Document, 2006.
  
  if (!requireNamespace('mclust')) {
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
  
  if (ClusterNo<2){
    warning("ClusterNo should be an integer > 2. Now, all of your data is in one cluster.")
    if(is.null(nrow(Data))){# dann haben wir einen Vektor
      return(cls <- rep(1,length(Data)))
    }else{ # Matrix
      return(cls <- rep(1, nrow(Data)))
    }
  }
  
  res=mclust::Mclust(Data,G=ClusterNo,modelNames=mclust::mclust.options("emModelNames"),...)
  Cls=res$classification
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=res))
  }