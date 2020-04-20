FannyClustering=function(DataOrDistances,ClusterNo,PlotIt=FALSE,Standardization=TRUE,Data,...){
  
  if(missing(DataOrDistances)){
    DataOrDistances=Data
  }
  if(Standardization==1) Standardization=TRUE
  if(Standardization==0) Standardization=FALSE
  #author: MT, 04/2018
  requireNamespace('cluster')
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
	ClusterPlotMDS(Data,Cls2)
  }
    Cls=ClusterRename(Cls,DataOrDistances)
  return(list(Cls=Cls,Object=fan))
}