SOTAclustering=sotaClustering =function(Data,ClusterNo,PlotIt=FALSE,UnrestGrowth,...){
  # Cls=sotaClustering(Data,ClusterNo=2)
  # Self-organizing Tree Algorithm (SOTA)
  # liefert eine Klassenzuweisung
  # INPUT
  # Data[1:n,1:d]             Der Datensatz
  
  # ClusterNo  in soviele Cluster werden die daten eingeteilt
  
  # OUTPUT
  # Cls[1:n]                Clusterung der Daten
  # sotaObject         Object of sota Alorithm
  # Author: MT 04/2018
  
  if(missing(UnrestGrowth)){
  UnrestGrowth=TRUE
  }
  requireNamespace('clValid')
  if(isFALSE(UnrestGrowth))#maxCycles is number of iterations
	res=clValid::sota(Data,maxCycles = ClusterNo,unrest.growth=UnrestGrowth,...)
  else #maxCycles is number of of clusters+1
	res=clValid::sota(Data,maxCycles = ClusterNo-1,	unrest.growth=UnrestGrowth,...)
	
  Cls=res$clust
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
    
  if(PlotIt){
    requireNamespace('DataVisualizations')
    DataVisualizations::Plot3D(Data,Cls)
  }
  return(list(Cls=Cls,sotaObject=res))
}