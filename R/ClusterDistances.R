ClusterDistances=IntraClusterDistances=ClusterIntraDistances=function(FullDistanceMatrix,Cls,Names,PlotIt=FALSE){
  #
  # INPUT
  # FullDistanceMatrix    symmetric distance matrix
  # Cls                   numerical vector of k classes
  #
  # OPTIONAL
  # Names                 character vector naming k classes
  # PlotIt                Boolean
  #
  # OUTPUT
  # matrix [1:m,1:(k+1)] of k clusters, each columns consists of the distances in a cluster,
  # filled up with NaN at the end to be of the same length as the complete distance matrix.
  #
  #
  if(missing(Cls)){
    Cls=rep(1,nrow(FullDistanceMatrix))
  }
  if(!is.vector(Cls)){
    warning('ClusterDistances: Cls is not a vector. Calling as.numeric(as.character(Cls))')
    Cls=as.numeric(as.character(Cls))
  }
  
  if(nrow(FullDistanceMatrix)!=length(Cls)){
	stop('ClusterDistances: Dimensionality of distance matrix "FullDistanceMatrix" is not consistent with "Cls" classification vector')
  }
  
  if(!isSymmetric(unname(FullDistanceMatrix))){
	stop('ClusterDistances: Distance matrix "FullDistanceMatrix" is not symmetric. Please check this, e.g. DataVisualizations::Pixelmatrix.')
  }
  u=sort(unique(Cls))

  classdist=list(FullDistanceMatrix[upper.tri(FullDistanceMatrix,diag = F)])
  if(length(u)==1) return(unlist(classdist))
  # Does not work for clustersize==1!
  for(i in u){
    classdistcur=FullDistanceMatrix[Cls==i,Cls==i]
    distvec=classdistcur[upper.tri(classdistcur,diag = F)]
    classdist=c(classdist,list(distvec))
  }

    Intraclusterdistances=do.call(DataVisualizations::CombineCols,classdist)
    Intraclusterdistances=as.matrix(Intraclusterdistances)
    if(missing(Names)){
      colnames(Intraclusterdistances)=c('Full',paste0('Cluster',u))
    }else{
      if(length(u)!=length(Names)){
        warning('ClusterDistances: Length of Names has to be equal of length of unique Cls.')
        colnames(Intraclusterdistances)=c('Full',paste0('Cluster',Names))
      }else{
        colnames(Intraclusterdistances)=c('Full',Names)
      }
    }
    
    if(PlotIt){
      ggobject=DataVisualizations::MDplot(Intraclusterdistances,OnlyPlotOutput = TRUE)
      print(ggobject)
      return(list(ClusterDists=as.matrix(Intraclusterdistances),ggobject=ggobject))
    }

  return(Intraclusterdistances)
}
