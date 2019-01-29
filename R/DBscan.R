DBscan <-function(Data,Radius,minPts,PlotIt=FALSE,...){
#  res=kmeans(FCPS$Hepta$Data,7)
#  Cls=DBscan(FCPS$Hepta$Data,sqrt(min(res$withinss)))
# DBscan nach [Ester et al., 1996]
# INPUT
# Data[1:n,1:d]          der Datensatz 
# Radius                 eps,  Radius der R-kugel [Ester et al., 1996, p. 227],size of the epsilon neighborhood.
# OPTIONAL
# minPts                 In principle minimum number of points in the unit disk, if the unit disk is within the cluster (core) [Ester et al., 1996, p. 228].
#                        number of minimum points in the eps region (for core points). 
#                      Default is 5 points.
# OUTPUT List V with
# Cls[1:n]               Clusterung der Daten, Points which cannot be assigned to a cluster will be reported as members of the noise cluster with NaN.
# 
# author: MT 2017, 1.Editor MT 04/2018
#
# [Ester et al., 1996]  Ester, M., Kriegel, H.-P., Sander, J., & Xu, X.: A density-based algorithm for discovering clusters in large spatial databases with noise, Proc. Kdd, Vol. 96, pp. 226-231, 1996.


 if(is.null(nrow(Data))){# dann haben wir einen Vektor
    return(cls <- rep(1,length(Data)))
  }
  
if(missing(Radius)){  
  warning('The Radius (eps) parameter is missing but it is required in DBscan. Trying to estimate..')
  Radius=0.5*AdaptGauss::ParetoRadius(Data)
} 
  if(missing(minPts)){
    minPts=round(0.025*nrow(Data),0)
    warning('The minPts parameter is missing but it is required in DBscan. Trying to estimate..')
  }   
  
  requireNamespace('dbscan')
  liste=dbscan::dbscan(x = Data,eps = Radius,minPts = minPts,...)
  Cls=liste$cluster
  ind=which(Cls==0)
  # if(length(ind)>0)
  #   Cls[ind]=999
	#Cls=NormalizeCls(Cls)$normalizedCls
	#if(length(ind)>0)
	#  Cls[ind]=NaN
  Cls[!is.finite(Cls)]=0
	if(PlotIt){
	  requireNamespace('DataVisualizations')
	  Cls2=Cls
	  Cls2[Cls2==0]=999
	  DataVisualizations::plot3D(Data,Cls2)
	}
	
  return(list(Cls=Cls,DBscanObject=liste))

}
