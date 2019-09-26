EstimateRadiusByDistance=function(DistanceMatrix){
  requireNamespace('ABCanalysis')
  x=DistanceMatrix[lower.tri(DistanceMatrix, diag = FALSE)]
  xx=ABCanalysis::ABCRemoveSmallYields(x,0.5)
  x=xx$SubstantialData
  res=ABCanalysis::ABCanalysis(x)
  Radius=min(x[res$Aind])/max(x[res$Cind])
  return(Radius)
}