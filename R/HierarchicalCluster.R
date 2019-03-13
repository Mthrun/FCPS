HierarchicalCluster <-function(Data,ClusterNo=0,ClusterAlg="ward.D2",DistanceMethod="euclidean",ColorTreshold=0,...){
# HierarchicalCluster(Data)
# HierarchicalClusterDists(Data,0,"ward.D2",NULL,"cosine",100)
# Cls=HierarchicalCluster(Data,6,"ward.D2")
#  
# Zeichnet entweder ein Dendrogram oder liefert eine Klassenzuweisung
# INPUT
# Data[d,n]             Der Datensatz, Ohne NaNs!
# OPTIONAL
# ClusterNo  in soviele Cluster werden die daten eingeteilt, wenn dieser Wert 
#                       fehlt oder =0 gesetzt ist, wird ein Dendrogramm gezeichnet
# ClusterAlg  		      Methode der Clusterung: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".
# Distance               see  DistanceMatrix(), for example 'euclidean','sqEuclidean','mahalanobis','cityblock=manhatten','cosine','chebychev','jaccard','minkowski','manhattan','binary', 'canberra', 'maximum'. Any unambiguous substring can be given.
# ColorTreshold			     zeichnet Schnittlinie bei entsprechenden Dendrogram y-Achsenwerte (Hoehe), Hoehe der Linie wird als Skalar angegeben
# ...                   Nur Falls ClusterNo=0, plot argumente fuer as.dendrogramm, z.b.
# # leaflab             a string specifying how leaves are labeled. The default "perpendicular" write text vertically (by default).
#                        "textlike" writes text horizontally (in a rectangle), and 
#                        "none" suppresses leaf labels 
#                        s. ?as.dendrogramm
#  
#
# OUTPUT
# HierarchicalCluster      Hierarchische Clusterung der Daten
#
# Author: MT
#1.Editor: MT 07/2015, pdist fuer Distanzberechnung verwendet
# Example
# HierarchicalClusterDists(Data,0,"ward.D",NULL,"cosine",100, leaflab="none")  
# NOTE:
#Ward algorithm is directly correctly implemented in just Ward2, but rather that: (1) to get correct results with both implementations, use squared Euclidean distances with Ward1 and nonsquared Euclidean distances with Ward2; (2) to further make their output dendrograms comparable (identical), apply sq. root to fusion levels after Ward1 or square fusion levels after Ward2, before constructing dendrogram. 
 
  
#Distanzberechnung
#if (Distance=="euclidean"){
#	if(any(is.nan(Data),na.rm=TRUE)) {
#		Liste = naneucliddist(Data);
##		Y <- as.dist(Liste$distanceMatrix); #Umwandlung in Format, welches hc-clust akzeptiert
#    warning('NaNs in Data excluded!')
#	} 
#	else{
#		Y =dist(Data,method=Distance) #Fehler in pmatch(method, "euclidian") : Objekt 'Distance' nicht gefunden
#	}
#}
#else{
requireNamespace('parallelDist')
  Y=parallelDist::parDist(Data,method=DistanceMethod)#} #Case Corr und otherwiese
if(any(is.nan(Y),na.rm=TRUE)) {
stop('DistanceMethod with NaN in calculated. Please choose another DistanceMethod.')}
if(any(is.infinite(Y),na.rm=TRUE)) {
stop('DistanceMethod with infinites in calculated. Please choose another DistanceMethod.')}
#Clustering

hc <- hclust(Y,method=ClusterAlg); #hclust liefer in Matlab andere Werte (Aufruf: Z = linkage(Y,ClusterAlg))
if(DistanceMethod=='euclidean')
  DistanceMethod='Euclidean'
  
m=paste(ClusterAlg,"LinkCluster/ ",DistanceMethod," N=",nrow(as.matrix(Data)))
# Classification or Dendrogram
if (ClusterNo>0){
	return (cutree(hc,ClusterNo));	
	} 
else{
		x=as.dendrogram(hc)
    #plot(x, main=m,xlab="Anzahl N", ylab=DistanceMethod, sub=" ",leaflab ="none")
    plot(x, main=m,xlab="Number of Data Points N", ylab=DistanceMethod, sub=" ",...)
   # if(is.null(rownames(x))){
   #   plot(x, main=m,xlab="Anzahl N", ylab=DistanceMethod, sub=" ",leaflab ="none")
   # }else{
   #   plot(x, main=m,xlab="Anzahl N", ylab=DistanceMethod, sub=" ",leaflab =rownames(x))
   # }
		axis(1,col="black",las=1)
    if (ColorTreshold!=0){
			rect.hclust(hc, h=ColorTreshold,border="red")}
		else{
		#rect.hclust(hc, h=4*mean(hc$height),border="red")
		}
	}
}
