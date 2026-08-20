ClusterDendrogram=function(TreeOrDendrogram,ClusterNo,Colorsequence,main,xlab,ylab){
  # Affinity Propagation Clustering 
  #
  # INPUT
  # TreeOrDendrogram    Either object of hcclust defining the tree, third list element of hierarchical
  #                     cluster algorithms of this package or object of class dendrogram,  second list
  #                     element of hierarchical cluster algorithms.
  # ClusterNo           Number of clusters to search for
  # Colorsequence       Character vector of colors. Per default the colorsquence defined in the DataVisualization package is used
  # main,xlab,ylab      String. Title, xlabel and ylabel of plot
  # 
  # OUTPUT
  # Numerical vector defining the clustering of k clusters; this classification is the main output of the algorithm.
  # 
  # Author: MT
  if(ClusterNo<1){
    stop('ClusterNo has to be 1 or higher')
  }
  if(missing(main)){
    main='Name of Algorithm'
  }
  if(missing(ylab)){
    ylab="Ultrametric Portion of Distance"
  }
  if(missing(xlab)){
    xlab="No. of Data Points N"
  }
  
  
  if(!inherits(TreeOrDendrogram,"hclust")){
    if(inherits(TreeOrDendrogram,"dendrogram")){
      tryCatch({Tree=as.hclust(TreeOrDendrogram)},error=function(e){
        warning(e)
        warning('ClusterDendrogram: TreeOrDendrogram inherits class dendrogram but cannot be conversed to class hclust, cutree function may not work.')
        })
    }else{
      tryCatch({
        Tree=as.hclust(as.dendrogram(TreeOrDendrogram))
      },error=function(e){
        warning(e)
        warning('ClusterDendrogram: TreeOrDendrogram is neither of class hclust or dendrogram, cutree function may not work.')
      })
    }
  }else{
    Tree=TreeOrDendrogram
  }
  Cls = cutree(Tree, ClusterNo)
  x=as.dendrogram(Tree)

  # Get the right number of colors
  numberOfClasses <- length(unique(Cls))
  if(missing(Colorsequence)){
    if(requireNamespace("DataVisualizations")){
      cols= DataVisualizations::DefaultColorSequence[1:numberOfClasses]
    }
    else{
      stop('DataVisualizations package not loaded or installed. Please provide Colorsequence manually.')
    }
  }else{
    cols=Colorsequence
    if(length(cols)!=numberOfClasses){
      warning('Default color sequence is used, because the number of colors does not equal the number of clusters.')
      if(requireNamespace("DataVisualizations")){
        cols= DataVisualizations::DefaultColorSequence[1:numberOfClasses]
      }
      else{
        stop('DataVisualizations package not loaded or installed. Please provide Colorsequence manually.')
      }
    }
  }
  
  if(requireNamespace('dendextend',quietly = TRUE)){
    # What is the ordering of the cluster in dendrogram
    # from left to right
    Clstemp=Cls[order.dendrogram(x)]
    uniqueClasses <- unique(Clstemp)
    # Count cluster frequencies in the dendrogram order.
    countPerClass <- vapply(
      uniqueClasses,
      function(label) sum(Clstemp == label),
      integer(1)
    )
    
    # Assign the first color to the largest cluster, the second color
    # to the second-largest cluster, and so on, while retaining the
    # left-to-right branch order expected by dendextend.
    classesBySize=uniqueClasses[order(countPerClass,decreasing = TRUE)]
    cols_order=match(uniqueClasses,classesBySize)
    cols=cols[cols_order]
    # Branch colors with specific set of colors based on cluster frequency
    x=dendextend::set(x,"branches_k_color", k = ClusterNo,cols)
  }else{
    warning('dendextend package is missing. Simple dendrogram plot without colors is used.')
  }
  plot(x, main=main,xlab=xlab, ylab=ylab,sub=" ",leaflab ="none")
  axis(1,col="black",las=1)
  return(invisible(Cls))
}