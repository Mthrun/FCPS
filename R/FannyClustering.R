FannyClustering=function(DataOrDistances,ClusterNo,PlotIt=FALSE,Data,...){
  
  if(missing(DataOrDistances)){
    DataOrDistances=Data
  }

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
  
  fan=cluster::fanny(Input,k=ClusterNo,diss=diss,...)
  Cls=fan$clustering
  if(!is.null(rownames(DataOrDistances)))
    names(Cls)=rownames(DataOrDistances)
  else
    names(Cls)=1:nrow(DataOrDistances)
  
  Cls[!is.finite(Cls)]=0
  if(PlotIt){
    Cls2=Cls
    Cls2[Cls2==0]=999
    requireNamespace('DataVisualizations')
    DataVisualizations::Plot3D(DataPoints,Cls2)
  }
  return(list(Cls=Cls,ObjectFanny=fan))
}