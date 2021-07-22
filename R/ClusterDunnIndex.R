ClusterDunnIndex=function(Cls,DataOrDistances,DistanceMethod="euclidean",Silent=TRUE,...){
  
  if (isSymmetric(unname(DataOrDistances))) {
    DataDists = DataOrDistances
  } else{
    if(!Silent)
      print('Distances are not in a symmetric matrix, Datamatrix is assumed and parallelDist::parDist() ist called')
    
    if (!requireNamespace('parallelDist')) {
      message(
        'Subordinate parallelDist package is missing. dist() function of stats is used.
				Please install the package which is defined in "Suggests", if other distances than available in dist() or faster distance computation is necessary'
      )
      DataDists = as.matrix(dist(DataOrDistances, method = DistanceMethod))
    }else{
      DataDists = as.matrix(parallelDist::parDist(DataOrDistances, method = DistanceMethod,...))
    }
  }# end if(isSymmetric(DataOrDists))
  
  if(length(unique(Cls))>1){
   V=FCPS::ClusterCount(Cls)
   CountPerCluster=V$CountPerCluster
   indoutlier=which(CountPerCluster==1)
      if(length(indoutlier)==length(CountPerCluster)){
        warning("ClusterDunnIndex: All clusters have only one datapoint per cluster. Computation cannot be performed.")
        return(list(Dunn=NaN,IntraDist=NULL,InterDist=NULL))
      }
      if((length(indoutlier)+1)==length(CountPerCluster)){
        warning("ClusterDunnIndex: Too many clusters with one datapoint per cluster. Computation cannot be performed.")
        return(list(Dunn=NaN,IntraDist=NULL,InterDist=NULL))
      }
  intrac=ClusterIntraDistances(DataDists,Cls,PlotIt = F)
  interc=ClusterInterDistances(DataDists,Cls,PlotIt = F)
  
  #clear full distance matrix from first column
  ind1=which(colnames(intrac)=="Full")
  ind2=which(colnames(interc)=="Full")
  if(length(ind1)==1)
    intrac=intrac[,-ind1]
  
  if(length(ind2)==1)
    interc=interc[,-ind2]
	
  intrac[!is.finite(intrac)]=NaN
  interc[!is.finite(interc)]=NaN
	
  InnerDist = apply(FUN = max,MARGIN = 2,X = intrac,na.rm=T)

  InterDist = apply(FUN = min,MARGIN = 2,X = interc,na.rm=T)

  if(sum(is.finite(InnerDist))==0){
	  dunn <- NaN
  }else if(sum(is.finite(InterDist))==0){
	  dunn <- NaN
  } else if (max(InnerDist,na.rm = T) < 10^(-7)) {
    dunn <- NaN
  }
  else {
    dunn <- (min(InterDist,na.rm = T)/max(InnerDist,na.rm = T))
  }
  return(list(Dunn=dunn,IntraDist=InnerDist,InterDist=InterDist))
  }else{
    warning("ClusterDunnIndex: only one cluster given. Computation cannot be performed.")
    return(list(Dunn=NaN,IntraDist=NULL,InterDist=NULL))
  }
}