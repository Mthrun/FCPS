ClusterDendrogram=function(TreeOrDendrogram,ClusterNo,Colorsequence,main='Name of Algorithm'){
  if(ClusterNo<1){
    stop('ClusterNo has to be 1 or higher')
  }
  if(!inherits(TreeOrDendrogram,"hclust")){
    if(inherits(TreeOrDendrogram,"dendrogram")){
      tryCatch({Tree=as.hclust(TreeOrDendrogram)},error=function(e){
        warning(e)
        warning('ClusterDendrogram: TreeOrDendrogram inherits class dendrogram but cannot be conversed to class hclust. cutree may not work.')
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

  #get the right number of colors
  numberOfClasses <- length(unique(Cls))
  if(missing(Colorsequence)){
    cols= DataVisualizations::DefaultColorSequence[1:numberOfClasses]
  }else{
    cols=Colorsequence
    if(length(cols)!=numberOfClasses){
      warning('Default color sequence is used, because the number of colors does not equal the number of clusters.')
      cols=DataVisualizations::DefaultColorSequence[1:numberOfClasses]
    }
  }
  
  if(requireNamespace('dendextend')){
    #what is the ordering of the cluster in dendrogram
    # from left to right
    Clstemp=Cls[order.dendrogram(x)]
    uniqueClasses <- unique(Clstemp)
    #count frequency in that ordering
    countPerClass=list()
    for (i in uniqueClasses) {
      inClassI <- sum(Clstemp == uniqueClasses[i])
      countPerClass[[i]] = inClassI
    }
    names(countPerClass)=uniqueClasses
    countPerClass=unlist(countPerClass)

    #what would be the ordering of datra based on frequency
    data_order=order(countPerClass,decreasing = TRUE) #from highest frequency
    #what would be the orders of the branches
    unique_reordered=uniqueClasses[data_order]
    # fit that order to the colors
    cols_order = match(table = unique_reordered,uniqueClasses)
    cols=cols[cols_order]
    #branch colors with specific set of colors based on cluster frequency
    x=dendextend::set(x,"branches_k_color", k = ClusterNo,cols)
  }else{
    warning('dendextend package is missing. Simple dendrogram plot without colors is used.')
  }
  plot(x, main=main,xlab="No. of Data Points N", ylab="Ultrametric Portion of Distance",sub=" ",leaflab ="none")
  axis(1,col="black",las=1)
  return(invisible(Cls))
}