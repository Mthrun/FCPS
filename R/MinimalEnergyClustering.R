MinimalEnergyClustering <-function(DataOrDistances,ClusterNo=0,DistanceMethod="euclidean",ColorTreshold=0,Data,...){
# HierarchicalClusterDists(pDist)
# HierarchicalClusterDists(pDist,0,"ward.D2",100)
# Cls=HierarchicalClusterDists(pDist,6,"ward.D2")
  
# Zeichnet entweder ein Dendrogram oder liefert eine Klassenzuweisung
# INPUT
# pDist                 Distanzen eines Datensatzesueber DistanceMatrix()
# OPTIONAL
# ClusterNo  in soviele Cluster werden die daten eingeteilt, wenn dieser Wert 
#                       fehlt oder =0 gesetzt ist, wird ein Dendrogramm gezeichnet
# ColorTreshold			    zeichnet Schnittlinie bei entsprechende, Dendogram y-Achsenwerte (Hoehe), Hoehe der Linie wird als Skalar angegeben
#
# OUTPUT Liste mit
# HierarchicalCluster      Hierarchische Clusterung der Daten, falls ClusterNo angegeben
# Dendrogram
# Author: MT, 2019

#Clustering
  if(missing(DataOrDistances)){
    DataOrDistances=Data
  }
  
  if (!isSymmetric(unname(DataOrDistances))) {
    requireNamespace('parallelDist')
    pDist=as.dist(parallelDist::parDist(DataOrDistances,method=DistanceMethod))
  }else if(!inherits(DataOrDistances,'dist')){
    pDist=as.dist(DataOrDistances)
  }else{
    pDist=DataOrDistances
  }
  
  requireNamespace('energy')
	hc <- energy::energy.hclust(pDist)
	
	m=paste("Minimal Energy Clustering/ "," N=",nrow(as.matrix(pDist)))
	
# Classification or Dendrogram
	if (ClusterNo>0){
		Cls=cutree(hc,ClusterNo)
		Cls=ClusterRename(Cls,DataOrDistances)
		return (Cls)
	} 
	else{
		x=as.dendrogram(hc);plot(x, main=m,xlab="Number of Data Points N", ylab="Distance",sub=" ",leaflab ="none",...)
		axis(1,col="black",las=1)
		if (ColorTreshold!=0){
		  rect.hclust(hc, h=ColorTreshold,border="red")}		  
		else{
		}
		return(list(Cls=hc,Dedrogram=x))
	}
}



