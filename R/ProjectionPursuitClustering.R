ProjectionPursuitClustering=function(Data,ClusterNo,Type="MinimumDensity",PlotIt=FALSE,PlotSolution=FALSE,...){
    

    #author: MT, 04/2020
    requireNamespace('PPCI')

    switch(Type,
      'MinimumDensity'={
        out=PPCI::mddc(X=Data,K=ClusterNo,...)
        },
      'MaximumClusterbility'={out=PPCI::mcdc(X=Data,K=ClusterNo,...)},
      'NormalisedCut'={out=PPCI::ncutdc(X=Data,K=ClusterNo,...)},
      'KernelPCA'={
        if(!missing(ClusterNo))
          message('ProjectionPursuitClustering of Type KernelPCA does not require "ClusterNo" and will determine the number of clusters automatically.')
        requireNamespace('kernlab')
        x2=kernlab::kpca(Data,kernel="rbfdot",kpar=list(sigma=3))@rotated
        out=PPCI::ncuth(x2,...) 
      },{
        warning('Incorrect Option Selected')
        return('Incorrect Option Selected')
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