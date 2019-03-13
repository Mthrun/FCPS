DBSclusteringAndVisualization=function(DataOrDistances,ClusterNo=0,StructureType=TRUE,DistancesMethod=NULL,PlotTree=FALSE,PlotMap=TRUE){
  requireNamespace('DatabionicSwarm')
  if(is.null(DistancesMethod)){
    if (isSymmetric(DataOrDistances)) {
      DataDists = DataOrDistances
      requireNamespace('ProjectionBasedClustering')
      DataPoints=ProjectionBasedClustering::MDS(DataDists,OutputDimension = nrow(DataDists))$ProjectedPoints
      AnzVar = ncol(DataOrDistances)
      AnzData = nrow(DataOrDistances)
    }else{
      DataPoints=DataOrDistances
    }
    proj= DatabionicSwarm::Pswarm(DataOrDistance = DataOrDistances)
  }else{
    requireNamespace('parallelDist')
    DataDists=as.matrix(parallelDist::parallelDist(DataOrDistances,method = DistancesMethod))
    DataPoints=ProjectionBasedClustering::MDS(DataDists,OutputDimension = nrow(DataDists))$ProjectedPoints
    proj= DatabionicSwarm::Pswarm(DataOrDistance = DataDists)
  }
  generalizedUmatrix=DatabionicSwarm::GeneratePswarmVisualization(Data = DataPoints,ProjectedPoints = proj$ProjectedPoints,LC = proj$LC)
  if(ClusterNo==0){
    none=DBSclustering(1,DataOrDistance = DataOrDistances,BestMatches =generalizedUmatrix$Bestmatche,LC = generalizedUmatrix$LC,
                  StructureType = StructureType,PlotIt = PlotTree)
   GeneralizedUmatrix::plotTopographicMap(generalizedUmatrix$Umatrix,generalizedUmatrix$Bestmatches)
   Cls=NULL
  }else{
    if(is.null(DistancesMethod)){
      Cls=DBSclustering(ClusterNo,DataOrDistance = DataOrDistances,BestMatches =generalizedUmatrix$Bestmatche,LC = generalizedUmatrix$LC,
                        StructureType = StructureType,PlotIt = PlotTree)
    }else{
      Cls=DBSclustering(ClusterNo,DataOrDistance = DataDists,BestMatches =generalizedUmatrix$Bestmatche,LC = generalizedUmatrix$LC,
                        StructureType = StructureType,PlotIt = PlotTree)
    }
  }
  if(PlotMap){
    GeneralizedUmatrix::plotTopographicMap(generalizedUmatrix$Umatrix,generalizedUmatrix$Bestmatches,Cls = Cls)
  }
  return(list(Projection=proj,GeneralizedUmatrixOfSwarm=generalizedUmatrix,Cls=Cls,DataPoints=DataPoints))
}
