MoGclustering <-function(Data,ClusterNo=2,Type,PlotIt=FALSE,Silent=TRUE,...){
  # Cls <- MoGclustering(Data,ClusterNo);
  # MixtureOfGaussians (MoG) clustering using EM
  # 
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Number of clusters to search for
  #
  # OPTIONAL
  # Type            Choose Method. EM = Expectation Maximization
  # PlotIt            Boolean. Decision to plot or not
  # 
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of EMCluster::emgroup or EMCluster::starts.via.svd
  # 
  # MT 2017, upadate 2018, and 2021
  # IMPORTANT UPDATE: MoGclustering renamed to ModelBasedClustering MoG Clustering is now defined es Mixture of Gaussians based on EM This is a change contrary to my PhD thesis [Thrun, 2018]! Additionally density based clustering methods added.
  # 2021: integrating further algorithms strongly resembling EM

  
  if (ClusterNo<2){
    warning("ClusterNo should to be an integer > 2. Now, all of your data is in one cluster.")
    if(is.null(nrow(Data))){# dann haben wir einen Vektor
      return(cls <- rep(1,length(Data)))
    }else{ # Matrix
      return(cls <- rep(1, nrow(Data)))
    }
  }#

  switch(Type,
    EM={
      if (!requireNamespace('EMCluster',quietly = TRUE)) {
        message(
          'Subordinate clustering package (EMCluster) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
        )
        return(
          list(
            Cls = rep(1, nrow(Data)),
            Object = "Subordinate clustering package (EMCluster) is missing.
                Please install the package which is defined in 'Suggests'."
          )
        )
      }
      out=EMCluster::starts.via.svd(Data, nclass = ClusterNo, method = c("em"),
                                    EMC = EMCluster::.EMC,...)
      Cls=as.vector(out$class)
    },
    kmeans={
      if (!requireNamespace('EMCluster',quietly = TRUE)) {
        message(
          'Subordinate clustering package (EMCluster) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
        )
        return(
          list(
            Cls = rep(1, nrow(Data)),
            Object = "Subordinate clustering package (EMCluster) is missing.
                Please install the package which is defined in 'Suggests'."
          )
        )
      }
      out=EMCluster::emgroup(x = Data,nclass = ClusterNo,EMC = EMCluster::.EMC,...)
      Cls=as.vector(out$class)
    },
    mvnormalmixEM = {  
      out = mixtools::mvnormalmixEM(Data, k=ClusterNo,verb = !Silent, ...)
      Cls = apply(out$posteriors, 1, which.max)
      # Error can arize sometimes at least!:
      # Error in qr.solve(a) : singular matrix 'a' in solve 
      # Why?
    },
    mvnpEM = {  
      out = mixtools::mvnpEM(Data, mu0=ClusterNo, verb=!Silent, ...)
      Cls = apply(out$posteriors, 1, which.max)
    },
    npEM = {  
      out = mixtools::npEM(Data, mu0=ClusterNo, verb=!Silent, ...)
      Cls = apply(out$posteriors, 1, which.max)
    },
    
    {
      stop('Please choose either "kmeans","EM", "mvnormalmixEM","mvnpEM" or "npEM".')
    }
  )
  Cls=ClusterRename(Cls,Data)
  
  if(isTRUE(PlotIt)){
    ClusterPlotMDS(Data,Cls)
  }
  return(list(Cls=Cls,Object=out))
  }