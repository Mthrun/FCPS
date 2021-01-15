DatabionicSwarmClustering=function(DataOrDistances,ClusterNo=0,StructureType=TRUE,DistancesMethod=NULL,PlotTree=FALSE,PlotMap=FALSE,PlotIt=FALSE,Data){
  # INPUT
  # DataOrDistances[1:n,1:d]    Either nonsymmetric [1:n,1:d] datamatrix of n cases and d features or
  #                             symmetric [1:n,1:n] distance matrix
  #
  # OPTIONAL
  # ClusterNo         Number of Clusters, if zero a the topographic map is ploted. Number of valleys
  #                   equals number of clusters.
  # StructureType     Either TRUE or FALSE, has to be tested against the visualization. If colored
  #                   points of clusters a divided by mountain ranges, parameter is incorrect.
  # DistancesMethod   Optional, if data matrix given, annon Euclidean distance can be selected
  # PlotTree          Optional, if TRUE: dendrogram is plotted.
  # PlotMap           Optional, if TRUE: topographic map is plotted.
  # PlotIt            Boolean. default: FALSE, If TRUE and dataset of [1:n,1:d] dimensions then a plot of
  #                   the first three dimensions of the dataset with colored three-dimensional data points
  #                   defined by the clustering stored in \code{Cls} will be generated
  #
  # OUTPUT
  # Cls               1:n numerical vector of numbers defining the classification as the main output of the
  #                   clustering algorithm for the n cases of data. It has k unique numbers representing the
  #                   arbitrary labels of the clustering.
  # Object            List of further output of DBS.
  #
  # Author: MT
  
  if(StructureType==1) StructureType=TRUE
  if(StructureType==0) StructureType=FALSE
  
  if(missing(DataOrDistances)){
    DataOrDistances=Data
  }
  if(!is.matrix(DataOrDistances)){
    warning('Argument "DataOrDistances" is not a matrix. Calling "as.matrix()"...')
    DataOrDistances=as.matrix(DataOrDistances)
  }
  generalizedUmatrix=NULL # Init in case of no plotting
  DataPoints=NULL  # Init in case of no plotting
  Cls = rep(1, nrow(DataOrDistances))#init
  
  if (!requireNamespace('DatabionicSwarm',quietly = TRUE)){
    message(
      'Subordinate clustering package (DatabionicSwarm) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = Cls,
        Object = "Subordinate clustering package (DatabionicSwarm) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }

  message("Operator: Computing nonlinear projection using the swarm.")
  if(is.null(DistancesMethod)){
    if (isSymmetric(unname(DataOrDistances))) {
      DataDists = DataOrDistances
      if(requireNamespace("ProjectionBasedClustering",quietly = TRUE)){
        DataPoints=ProjectionBasedClustering::MDS(DataDists,OutputDimension = nrow(DataDists)-1)$ProjectedPoints
        AnzVar = ncol(DataOrDistances)
        AnzData = nrow(DataOrDistances)
      }
      else{
        stop('ProjectionBasedClustering package not loaded or installed.')
      }#end requireNamespace("ProjectionBasedClustering)
      
    }else{
      DataPoints=DataOrDistances
    }#end isSymmetric(unname(DataOrDistances)
    if(requireNamespace("DatabionicSwarm",quietly = TRUE)){
      proj= DatabionicSwarm::Pswarm(DataOrDistance = DataOrDistances)
    }
    else{
      stop('DatabionicSwarm package not loaded or installed.')
    }# end requireNamespace("DatabionicSwarm"
    
  }else{
    requireNamespace('parallelDist')
    DataDists=as.matrix(parallelDist::parallelDist(DataOrDistances,method = DistancesMethod))
    proj= DatabionicSwarm::Pswarm(DataOrDistance = DataDists)
  }#end is.null(DistancesMethod)

  if(ClusterNo==0){
    if(isTRUE(PlotTree)){
      message("Operator: Generating dendrogram.")
      Cls=DatabionicSwarm::DBSclustering(1,DataOrDistance = DataOrDistances,BestMatches =TwoD_Points,LC = LC,
                                         StructureType = StructureType,PlotIt = PlotTree)
    }#end isTRUE(PlotTree
    if(!is.null(DistancesMethod))    
      DataPoints=ProjectionBasedClustering::MDS(DataDists,OutputDimension = nrow(DataDists))$ProjectedPoints
    
    generalizedUmatrix=DatabionicSwarm::GeneratePswarmVisualization(Data = DataPoints,ProjectedPoints = proj$ProjectedPoints,LC = proj$LC)
    
    if (requireNamespace('GeneralizedUmatrix',quietly = TRUE)){
      message("Operator: Generating topographic map of high-dimensional structures.")
      GeneralizedUmatrix::TopviewTopographicMap(GeneralizedUmatrix = generalizedUmatrix$Umatrix,BestMatchingUnits = generalizedUmatrix$Bestmatches)
    }else{
      message(
        'Subordinate clustering package (GeneralizedUmatrix) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".Setting "PlotMap=FALSE"'
      )
      PlotMap=FALSE
    }#end requireNamespace('GeneralizedUmatrix'
  }else{#ClusterNo!=0
    message("Operator: Clustering the dataset.")
    TwoD_Points=proj$ProjectedPoints
    # Make sure that grid dimensions are correct, even if GeneralizedUmatrix is not computed which is not necessary for clustering.
    LC=c()
    LC[1]=ceiling(max(TwoD_Points[, 1])+1)
    LC[2]=ceiling(max(TwoD_Points[, 2])+1)
 
    if(is.null(DistancesMethod)){
      Cls=DatabionicSwarm::DBSclustering(ClusterNo,DataOrDistance = DataOrDistances,BestMatches =TwoD_Points,LC = LC,
                        StructureType = StructureType,PlotIt = PlotTree)
    }else{
      Cls=DatabionicSwarm::DBSclustering(ClusterNo,DataOrDistance = DataDists,BestMatches =TwoD_Points,LC = LC,
                        StructureType = StructureType,PlotIt = PlotTree)
    }#end is.null(DistancesMethod))
    Cls=ClusterRename(Cls,DataOrDistances)
  }# end ClusterNo==0
  if (isTRUE(PlotMap)) {
    if (ClusterNo != 0) {
      message("Operator: Generating topographic map of high-dimensional structures.")
      generalizedUmatrix = DatabionicSwarm::GeneratePswarmVisualization(
        Data = DataPoints,
        ProjectedPoints = proj$ProjectedPoints,
        LC = proj$LC
      )
    }#end ClusterNo != 0 #otherwise it already exists
    if (ClusterNo == 0) {
      #plotting is already performed in order to derive the number of clusters
      #GeneralizedUmatrix::plotTopographicMap(generalizedUmatrix$Umatrix,generalizedUmatrix$Bestmatches)
    } else{
      if (requireNamespace('GeneralizedUmatrix', quietly = TRUE)) {
        GeneralizedUmatrix::plotTopographicMap(generalizedUmatrix$Umatrix,
                                               generalizedUmatrix$Bestmatches,
                                               Cls = Cls)
      }else{
        message(
          'Subordinate clustering package (GeneralizedUmatrix) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".Setting "PlotMap=FALSE"'
        )
        PlotMap=FALSE
      }#end !requireNamespace('GeneralizedUmatrix',
    }# end ClusterNo == 0)
  }#end PlotMap
  if(PlotIt){
	  ClusterPlotMDS(DataOrDistances,Cls)
  }
  
  return(list(Cls=Cls,Object=list(Projection=proj,GeneralizedUmatrixOfSwarm=generalizedUmatrix,Call=match.call(),DataPoints=DataPoints)))
}
