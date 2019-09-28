PAMclustering=PAMClustering=function(DataOrDistances,ClusterNo,PlotIt=FALSE,Standardization=TRUE,Data,...){
  if(missing(DataOrDistances)){
    DataOrDistances=Data
  }
  if(Standardization==1) Standardization=TRUE
  if(Standardization==0) Standardization=FALSE
  #author: MT, 04/2018
  requireNamespace('cluster')
      if (isSymmetric(DataOrDistances)) {
      Input = as.dist(DataOrDistances)
      requireNamespace('ProjectionBasedClustering')
      DataPoints=ProjectionBasedClustering::MDS(DataOrDistances,OutputDimension = 3)$ProjectedPoints
      AnzVar = ncol(DataOrDistances)
      AnzData = nrow(DataOrDistances)
	  diss =TRUE
    }else{
      DataPoints=DataOrDistances
	  Input=DataOrDistances
	  diss =FALSE
    }
	
  pam=cluster::pam(x=Input,k=ClusterNo,diss=diss,stand=Standardization,...)
  Cls=pam$clustering
  if(!is.null(rownames(DataOrDistances)))
    names(Cls)=rownames(DataOrDistances)
  else
    names(Cls)=1:nrow(DataOrDistances)
  
  if(PlotIt){
    requireNamespace('DataVisualizations')
    DataVisualizations::Plot3D(DataPoints,Cls)
  }
  return(list(Cls=Cls,ObjectPAM=pam))
}