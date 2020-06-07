ProjectionPursuitClustering=function(Data,ClusterNo,Type="MinimumDensity",PlotIt=FALSE,PlotSolution=FALSE,...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Number of clusters to search for
  # 
  # OPTIONAL
  # Type              Either MinimumDensity, MaximumClusterbility or NormalisedCut.
  # PlotIt            Boolean. Decision to plot or not
  # PlotSolution      Plots the partioning solution as a tree as described in 
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of PPCI::ncuth algorithm
  #
  # Author: MT, 04/2020
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
    switch(Type,
      'MinimumDensity'={
        out=PPCI::mddc(X=Data,K=ClusterNo,...)
        },
      'MaximumClusterbility'={out=PPCI::mcdc(X=Data,K=ClusterNo,...)},
      'NormalisedCut'={out=PPCI::ncutdc(X=Data,K=ClusterNo,...)},
      'KernelPCA'={
        if(!missing(ClusterNo)){
          message('ProjectionPursuitClustering of type KernelPCA does not require "ClusterNo" and will determine the number of clusters automatically.')
        }
        
        if(requireNamespace("kernlab")){
          x2=kernlab::kpca(Data,kernel="rbfdot",kpar=list(sigma=3))@rotated
        }
        else{
          stop('kernlab package not loaded or installed.')
        }
        out=PPCI::ncuth(x2,...) 
      },{
        warning('Incorrect option selected')
        return('Incorrect option selected')
      }
    )
  #  out=out
   
    Cls=out$cluster

    if(!is.null(rownames(Data)))
      names(Cls)=rownames(Data)
    else
      names(Cls)=1:nrow(Data)
    
    if(isTRUE(PlotIt)){
       ClusterPlotMDS(Data,Cls)
    }
    if(isTRUE(PlotSolution)){
      plot(out)
    }
	Cls=ClusterRename(Cls,Data)
    return(list(Cls=Cls,Object=out))
  }