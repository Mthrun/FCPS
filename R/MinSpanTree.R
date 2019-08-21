MinSpanTree <-
function(DataOrDistance,isDistance=FALSE){
# MST=MinSpanTree(Data)
# Zeichnet einen 2 Dimensionalen Minimal spanning Tree
#
# INPUT
# DataOrDistance[d,d]   Der Datensatz oder die Distanzmatrix
# 
# Optional
# isDistance			#Setze gleich True fals distanz
# 
# OUTPUT
# TR                    an object of class spantree which is a list with two vectors, 
#                       each of length n-1. The number of links in a tree is one less 
#                       the number of observations, and the first item is omitted
# Depths                Depths of nodes
#
# Author: MT
  
#requireNamespace('vegan')
#if(require(vegan)){library(vegan)}else{install.packages("vegan")
#library(vegan)}
requireNamespace('vegan')
if(isDistance){
AllDists=DataOrDistance
}else{
AllDists =dist(DataOrDistance,method = "euclidean")
}
tr=vegan::spantree(AllDists);

plot(tr,  cmdscale(AllDists))
return(list(TR=tr,Depths=vegan::spandepth(tr)))
}

