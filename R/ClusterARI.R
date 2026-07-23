ClusterARI=function(Cls1,Cls2,Fast=TRUE){
  if(isTRUE(Fast)){
    if(requireNamespace("mclust",quietly = TRUE)){
      ari=mclust::adjustedRandIndex(Cls1, Cls2)
      
      return(ari)
    }else{
      warning("ClusterARI:: Subordinate  package (mclust) is missing.
                  Please install the package which is defined in 'Suggests'.")
      return(
        "Subordinate  package (mclust) is missing.
                  Please install the package which is defined in 'Suggests'."
      )
      
      
    }
  }else{
    if(requireNamespace("partitionComparison",quietly = TRUE)){
    ari=partitionComparison::adjustedRandIndex(new("Partition", Cls1),
                                           new("Partition", Cls2))
    
    return(ari)
    }else{
      warning("ClusterARI:: Subordinate  package (partitionComparison) is missing.
                  Please install the package which is defined in 'Suggests'.")
      return(
       "Subordinate  package (partitionComparison) is missing.
                  Please install the package which is defined in 'Suggests'."
        )
    
    
    }
  }
}