DatabionicSwarmClustering=DBSclusteringAndVisualization=function(DataOrDistances,ClusterNo=0,StructureType=TRUE,DistancesMethod=NULL,PlotTree=FALSE,PlotMap=FALSE,Data){
  requireNamespace('DatabionicSwarm')
  if(StructureType==1) StructureType=TRUE
  if(StructureType==0) StructureType=FALSE
  if(missing(DataOrDistances)){
    DataOrDistances=Data
  }
  if(!is.matrix(DataOrDistances)){
    warning('Argument "DataOrDistances" is not a matrix. Calling "as.matrix()"...')
    DataOrDistances=as.matrix(DataOrDistances)
  }
  generalizedUmatrix=NULL #init in case of no plotting
  DataPoints=NULL  #init in case of no plotting
  message("Operator: Computing nonlinear projection using the swarm.")
  if(is.null(DistancesMethod)){
    if (isSymmetric(unname(DataOrDistances))) {
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
    proj= DatabionicSwarm::Pswarm(DataOrDistance = DataDists)
  }

  if(ClusterNo==0){
    if(PlotTree){
      message("Operator: Generating Dendrogram.")
      Cls=DatabionicSwarm::DBSclustering(1,DataOrDistance = DataOrDistances,BestMatches =TwoD_Points,LC = LC,
                                         StructureType = StructureType,PlotIt = PlotTree)
    }
    if(!is.null(DistancesMethod))    
      DataPoints=ProjectionBasedClustering::MDS(DataDists,OutputDimension = nrow(DataDists))$ProjectedPoints
   
   message("Operator: Generating toppgrahpic map of high-dimensional structures.")
   generalizedUmatrix=DatabionicSwarm::GeneratePswarmVisualization(Data = DataPoints,ProjectedPoints = proj$ProjectedPoints,LC = proj$LC)
   GeneralizedUmatrix::plotTopographicMap(generalizedUmatrix$Umatrix,generalizedUmatrix$Bestmatches)
   
  }else{
    message("Operator: Clustering the dataset.")
    TwoD_Points=proj$ProjectedPoints
    #Make sure that grid dimensions are correct, even if generalized umatrix is not computed which is not necessary for clustering.
    LC=c()
    LC[1]=ceiling(max(TwoD_Points[, 1])+1)
    LC[2]=ceiling(max(TwoD_Points[, 2])+1)
 
    if(is.null(DistancesMethod)){
      Cls=DatabionicSwarm::DBSclustering(ClusterNo,DataOrDistance = DataOrDistances,BestMatches =TwoD_Points,LC = LC,
                        StructureType = StructureType,PlotIt = PlotTree)
    }else{
      Cls=DatabionicSwarm::DBSclustering(ClusterNo,DataOrDistance = DataDists,BestMatches =TwoD_Points,LC = LC,
                        StructureType = StructureType,PlotIt = PlotTree)
    }
  }
  if(PlotMap){
    if(ClusterNo!=0){
      message("Operator: Generating topographic map of high-dimensional structures.")
      generalizedUmatrix=DatabionicSwarm::GeneratePswarmVisualization(Data = DataPoints,ProjectedPoints = proj$ProjectedPoints,LC = proj$LC)
    } 
    GeneralizedUmatrix::plotTopographicMap(generalizedUmatrix$Umatrix,generalizedUmatrix$Bestmatches,Cls = Cls)
  }
  return(list(Cls=Cls,Object=list(Projection=proj,GeneralizedUmatrixOfSwarm=generalizedUmatrix,Call=match.call(),DataPoints=DataPoints)))
}
