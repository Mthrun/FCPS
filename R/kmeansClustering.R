kmeansClustering <-function(Data,ClusterNo=2,Centers=NULL,method='LBG',PlotIt=FALSE,Verbose=0){
# Cls <- kmeansClustering(Data,ClusterNo,Verbose);
# call R's k-means method, best of 10 repetitions, each max. 100 optimizing steps
# INPUT
# Data[1:n]                  der Datensatz in Zeilenvektoren 
# ClusterNo  in soviele Cluster werden die daten eingeteilt
#
# OPTIONAL
# Centers                default(NULL), a set of initial (distinct) cluster centres
# method                 kind of kmeans algorithm, choose one of following strings
#                       "Hartigan": Hartigan, J. A. and Wong, M. A.. A K-means clustering algorithm. Applied Statistics 28, 100–108, 1979.
#                       "LBG": Linde,Y.,Buzo,A.,Gray,R.M., An algorithm for vector quantizer design. IEEE Transactions on Communications, COM-28, 84-95, 1980
# Verbose               '0' or '1' for a documentation of repetitions, default: Verbose==0
#
#
# OUTPUT List V with
# Cls[1:n]                k-means Clusterung der Daten
# SumDistsToCentroids     Vector of within-cluster sum of squares, one component per cluster
# Centroids               The final cluster centers.
# ALU 2014, MT 2016
# angepasst an Mdbt und Doku standards
  if (ClusterNo<2){
    warning("ClusterNo should to be an integer > 2. Now, all of your data is in one cluster.")
    if(is.null(nrow(Data))){# dann haben wir einen Vektor
      return(cls <- rep(1,length(Data)))
    }else{ # Matrix
      return(cls <- rep(1, nrow(Data)))
    }
  }
  
if(!is.null(Centers))  
  ClusterNo= Centers  # fuer Backward Compatibility
  
  requireNamespace('cclust')

  if(method=='Hartigan'){
  	c=kmeans(Data,ClusterNo,iter.max = 100);
  	if(Verbose==1){print(c);}
  	if(PlotIt){
  	  requireNamespace('DataVisualizations')
  	  DataVisualizations::Plot3D(Data,as.vector(c$cluster))
  	}
  	return(list(Cls=as.vector(c$cluster),SumDistsToCentroids=c$withinss,Centroids=c$centers))
  }
  if(method=='LBG'){
#    requireNamespace('cclust')
    res=cclust::cclust(x=Data,centers=ClusterNo,method='kmeans')
    SSE=res$withinss
    # s. http://epub.wu.ac.at/1542/1/document.pdf p.5
    # An examination of indexes for determining the number of clusters in binary data sets
    # Weingessel, Andreas and Dimitriadou, Evgenia and Dolnicar, Sara (1999)
    if(PlotIt){
      requireNamespace('DataVisualizations')
      DataVisualizations::Plot3D(Data,res$cluster)
    }
    return(list(Cls=res$cluster,SumDistsToCentroids=SSE,Centroids=res$centers))
  }
}
