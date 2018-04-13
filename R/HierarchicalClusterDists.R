HierarchicalClusterDists <-function(pDist,ClusterNo=0,ClusterAlg="ward.D2",ColorTreshold=0){
# HierarchicalClusterDists(pDist)
# HierarchicalClusterDists(pDist,0,"ward.D2",100)
# Cls=HierarchicalClusterDists(pDist,6,"ward.D2")
  
# Zeichnet entweder ein Dendrogram oder liefert eine Klassenzuweisung
# INPUT
# pDist                 Distanzen eines Datensatzesueber DistanceMatrix()
# OPTIONAL
# ClusterNo  in soviele Cluster werden die daten eingeteilt, wenn dieser Wert 
#                       fehlt oder =0 gesetzt ist, wird ein Dendrogramm gezeichnet
# ClusterAlg			      Methode der Clusterung: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".
# ColorTreshold			    zeichnet Schnittlinie bei entsprechende, Dendogram y-Achsenwerte (Hoehe), Hoehe der Linie wird als Skalar angegeben
#
# OUTPUT
# HierarchicalCluster      Hierarchische Clusterung der Daten, falls ClusterNo angegeben

# Author: MT
#Update 11.09.2014, "ward" zu "ward.D" ge?ndert f?r R 3.1.x, Tippfehler in Kommentaren korrigiert (Raphael P?bst)

#Clustering
  pDist=as.dist(pDist)
	hc <- hclust(pDist,method=ClusterAlg); #liefert teilweise andere Werte wie Z = linkage(Y,ClusterAlg);
	
	m=paste(ClusterAlg,"LinkCluster/ "," N=",nrow(as.matrix(pDist)))
	
# Classification or Dendrogram
	if (ClusterNo>0){
		return (cutree(hc,ClusterNo));
	} 
	else{
		x=as.dendrogram(hc);plot(x, main=m,xlab="Number of Data Points N", ylab="Distance",sub=" ",leaflab ="none")
		axis(1,col="black",las=1)
		if (ColorTreshold!=0){
		  rect.hclust(hc, h=ColorTreshold,border="red")}		  
		else{
		  #rect.hclust(hc, h=4*mean(hc$height),border="red")
		}
		return(hc)
	}
}



