ClusterRename=function(Cls,DataOrDistances){
  tryCatch({#make sure cls is given back
    if(missing(DataOrDistances)){
      warning('ClusterRename: DataOrDistances is missing' )
      return(Cls)
    }
    if(!is.vector(Cls)){
      warning('ClusterRename: Cls is not a vector. Calling as.numeric(as.character(Cls))')
      Cls=as.numeric(as.character(Cls))
    }
    if(nrow(DataOrDistances)!=length(Cls)){
      warning('ClusterRename: DataOrDistances number of rows does not equal length of Cls.Nothing is done' )
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