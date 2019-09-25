OPTICSclustering=function(Data, MaxRadius,RadiusThreshold, minPts = 5, PlotIt=FALSE,...){
  
  requireNamespace('dbscan')
  
  if(missing(MaxRadius)){  
    warning('The MaxRadius (eps) parameter is missing but it is required in OPTICS Trying to estimate..')
	requireNamespace('DataVisualizations')
    MaxRadius=0.5*DataVisualizations::ParetoRadiusV2(Data)
  } 
  if(missing(RadiusThreshold)){  
    warning('The RadiusThreshold (eps_cl) parameter is missing but it is required in OPTICS Trying to estimate..')
    RadiusThreshold=0.9*MaxRadius
  } 
  if(missing(minPts)){
    minPts=round(0.025*nrow(Data),0)
    warning('The minPts parameter is missing but it is required in DBscan. Trying to estimate..')
  }  
  out=dbscan::optics(Data,eps=MaxRadius,minPts=minPts,...)
  OPTICScobject=dbscan::extractDBSCAN(out, eps_cl = RadiusThreshold)

  Cls=OPTICScobject$cluster
  Cls[!is.finite(Cls)]=0
  if(!is.null(rownames(Data))){
    names(Cls)=rownames(Data)
  }
  
  if(PlotIt){
    requireNamespace('DataVisualizations')
    DataVisualizations::Plot3D(Data,Cls)
  }
  return(list(Cls=Cls,OPTICScobject=OPTICScobject))
}