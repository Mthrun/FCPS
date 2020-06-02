FannyClustering=function(DataOrDistances,ClusterNo,PlotIt=FALSE,Standardization=TRUE,...){
  #
  # INPUT
  # DataOrDistances[1:n,1:d]    Dataset with n observations and d features or distance matrix with size n
  # ClusterNo                   Number of clusters to search for
  # PlotIt                      Boolean. Decision to plot or not
  # Standardization             Boolean. Decision of use of standardization or not.
  #
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # fanObject         Object of fanny algorithm
  #
  # Author: MT, 04/2018
  if (!requireNamespace('cluster')) {
    message(
      'Subordinate clustering package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(DataOrDistances)),
        Object = "Subordinate clustering package is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  # if(missing(DataOrDistances)){
  #   DataOrDistances=Data
  # }
  if(Standardization==1) Standardization=TRUE
  if(Standardization==0) Standardization=FALSE

    if (unname(isSymmetric(DataOrDistances))) {
      Input = as.dist(DataOrDistances)
      AnzVar = ncol(DataOrDistances)
      AnzData = nrow(DataOrDistances)
	  diss =TRUE
    }else{
	  Input=DataOrDistances
	  diss =FALSE
    }
  
  fan=cluster::fanny(Input,k=ClusterNo,diss=diss,stand=Standardization,...)
  Cls=fan$clustering
  if(!is.null(rownames(DataOrDistances)))
    names(Cls)=rownames(DataOrDistances)
  else
    names(Cls)=1:nrow(DataOrDistances)
  
  Cls[!is.finite(Cls)]=0
  if(PlotIt){
    Cls2=Cls
    Cls2[Cls2==0]=999
	ClusterPlotMDS(DataOrDistances,Cls2)
  }
    Cls=ClusterRename(Cls,DataOrDistances)
  return(list(Cls=Cls,Object=fan))
}