ClusterRename=function(Cls,DataOrDistances){
  #
  # INPUT
  # Cls                1:n numerical vector of numbers defining the classification as the main
  #                    output of the clustering algorithm for the n cases of data. It has k unique
  #                    numbers representing the arbitrary labels of the clustering.
  # DataOrDistances    Either nonsymmetric [1:n,1:d] datamatrix of n cases and d features or
  #                    symmetric [1:n,1:n] distance matrix
  # 
  # OUTPUT
  # Cls[1:n] numerical vector named after the row names of data
  # 
  tryCatch({    # Make sure cls is given back
    if(missing(DataOrDistances)){
      warning('ClusterRename: DataOrDistances is missing' )
      return(Cls)
    }
    if(!is.vector(Cls)){
      warning('ClusterRename: Cls is not a vector. Calling as.numeric(as.character(Cls))')
      Cls=as.numeric(as.character(Cls))
    }
    if(nrow(DataOrDistances)!=length(Cls)){
      warning('ClusterRename: DataOrDistances number of rows does not equal length of Cls. Nothing is done' )
      return(Cls)
    }
  
    if(!is.null(rownames(DataOrDistances))){
      names(Cls)=rownames(DataOrDistances)
    }else{
      names(Cls)=1:nrow(DataOrDistances)
    }
  },error=function(e){
    warning(paste('ClusterRename:',e))
  })
  
  return(Cls)
}