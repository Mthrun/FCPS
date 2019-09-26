DBSclusteringAndVisualization=function(DataOrDistances,ClusterNo=0,StructureType=TRUE,DistancesMethod=NULL,PlotTree=FALSE,PlotMap=FALSE,Data){
  requireNamespace('DatabionicSwarm')
  if(StructureType==1) StructureType=TRUE
  if(StructureType==0) StructureType=FALSE
  if(missing(DataOrDistances)){
    DataOrDistances=Data
  }
  
  if(is.null(DistancesMethod)){
    if (isSymmetric(DataOrDistances)) {
      DataDists = DataOrDistances
      requireNamespace('ProjectionBasedClustering')
      DataPoints=ProjectionBasedClustering::MDS(DataDists,OutputDimension = nrow(DataDists)-1)$ProjectedPoints
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
    none=DatabionicSwarm::DBSclustering(1,DataOrDistance = DataOrDistances,BestMatches =generalizedUmatrix$Bestmatche,LC = generalizedUmatrix$LC,
                  StructureType = StructureType,PlotIt = PlotTree)
   GeneralizedUmatrix::plotTopographicMap(generalizedUmatrix$Umatrix,generalizedUmatrix$Bestmatches)
   Cls=NULL
  }else{
    if(is.null(DistancesMethod)){
      Cls=DatabionicSwarm::DBSclustering(ClusterNo,DataOrDistance = DataOrDistances,BestMatches =generalizedUmatrix$Bestmatche,LC = generalizedUmatrix$LC,
                        StructureType = StructureType,PlotIt = PlotTree)
    }else{
      Cls=DatabionicSwarm::DBSclustering(ClusterNo,DataOrDistance = DataDists,BestMatches =generalizedUmatrix$Bestmatche,LC = generalizedUmatrix$LC,
                        StructureType = StructureType,PlotIt = PlotTree)
    }
  }
  if(PlotMap){
    GeneralizedUmatrix::plotTopographicMap(generalizedUmatrix$Umatrix,generalizedUmatrix$Bestmatches,Cls = Cls)
  }
  return(list(Cls=Cls,DBSobject=list(Projection=proj,GeneralizedUmatrixOfSwarm=generalizedUmatrix,DataPoints=DataPoints)))
}
