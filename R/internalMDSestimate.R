internalMDSestimate=function(Distances){
  #smacof is not fast enough
  s=c()
  #fast mds
  for(i in 1:(nrow(Distances)-1)){
    s[i]= suppressWarnings(ProjectionBasedClustering::MDS(Distances,OutputDimension = i)$Stress)
    if(i>2)
      if(s[i]==s[i-1]& s[i]==s[i-2])
        break;
  }
  i=which.min(s)
  data=ProjectionBasedClustering::MDS(Distances,OutputDimension = i)$ProjectedPoints
  return(data)
}