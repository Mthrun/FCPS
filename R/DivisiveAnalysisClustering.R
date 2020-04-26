DivisiveAnalysisClustering <-function(DataOrDistances,ClusterNo,PlotIt=FALSE,Standardization=TRUE,Data,...){
  # Cls=DivisiveAnalysisClustering(Data,ClusterNo=2)
  # DivisiveAnalysisClustering (diana)
  # liefert eine Klassenzuweisung
  # INPUT
  # DataOrDistances[1:n,1:d]             Der Datensatz oder die Distanzmatrix [1:n,1:n]
  # ClusterNo  in soviele Cluster werden die daten eingeteilt
  
  # OUTPUT
  # Cls[1:n]                Clusterung der Daten
  # dianaObject         Object of sota Alorithm
  # Author: MT 04/2018
  if(missing(DataOrDistances)){
    DataOrDistances=Data
  }
  if(Standardization==1) Standardization=TRUE
  if(Standardization==0) Standardization=FALSE


  
  requireNamespace('cluster')
  if (isSymmetric(unname(DataOrDistances))) {
      Input = as.dist(DataOrDistances)
      AnzVar = ncol(DataOrDistances)
      AnzData = nrow(DataOrDistances)
	  diss =TRUE
    }else{
	  Input=DataOrDistances
	  diss =FALSE
    }
	
  res=cluster::diana(x=Input,diss =diss,stand=Standardization,...)
  if(length(ClusterNo)!=1){
    stop('ClusterNo has to be an numerical number not a vector of length higher than 1 or another object.')
  }
  if(ClusterNo>0){
    Cls=cutree(as.hclust(res), k = ClusterNo)
    
    if(PlotIt){
		ClusterPlotMDS(DataOrDistances,Cls)
    }
  }
  if(ClusterNo<=0){
    Cls=NULL
    plot(res)
    if(ClusterNo<0){
      warning(('ClusterNo cannot be a negativ number'))
    }
  }
    Cls=ClusterRename(Cls,DataOrDistances)
  return(list(Cls=Cls,Dendrogram=as.dendrogram(as.hclust(res)),Object=res))
}