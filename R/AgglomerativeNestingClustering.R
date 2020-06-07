AgglomerativeNestingClustering <-function(DataOrDistances,ClusterNo,PlotIt=FALSE,Standardization=TRUE,...){
  # Cls=DivisiveAnalysisClustering(Data,ClusterNo=2)$Cls
  # DivisiveAnalysisClustering (Diana)
  # Returns class assignment
  #
  # INPUT
  # DataOrDistances[1:n,1:d]    Dataset with n observations and d features or distance matrix with size n
  # ClusterNo                   Number of clusters to search for
  # 
  # OPTIONAL
  # PlotIt                      Boolean. Decision to plot or not.
  # Standardization             Boolean. If TRUE, then data gets standardized before calculating dissimilarities.
  #                                      If distances are given, this argument gets ignored
  # 
  # OUTPUT
  # Cls[1:n]            Clustering of data
  # Object         Object of sota algorithm
  # Dendrogram
  #
  # Author: MT 04/2018
  # if(missing(DataOrDistances)){
  #   DataOrDistances=Data
  # }
  if(Standardization==1) Standardization=TRUE
  if(Standardization==0) Standardization=FALSE


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
 
  if (isSymmetric(unname(DataOrDistances))) {
    Input = as.dist(DataOrDistances)
    requireNamespace('ProjectionBasedClustering')
    AnzVar = ncol(DataOrDistances)
    AnzData = nrow(DataOrDistances)
    diss =TRUE
  }else{
    Input=DataOrDistances
    diss =FALSE
  }
	
  res=cluster::agnes(x=Input,diss =diss,stand=Standardization,...)
  if(length(ClusterNo)!=1){
    stop('ClusterNo has to be a numerical number not a vector of length higher than 1 or another object.')
  }
  if(ClusterNo>0){
    Cls=cutree(as.hclust(res), k = ClusterNo)
    
    if(PlotIt){
		ClusterPlotMDS(DataOrDistances,Cls)
    }
	Cls=ClusterRename(Cls,DataOrDistances)
  }
  if(ClusterNo<=0){
    Cls=NULL
    plot(res)
    if(ClusterNo<0){
      warning(('ClusterNo cannot be a negativ number'))
    }
  }
  return(list(Cls=Cls, Object=res, Dendrogram=as.dendrogram(as.hclust(res))))
}