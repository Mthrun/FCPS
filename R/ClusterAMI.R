ClusterAMI=function(Cls1,Cls2){

    if(requireNamespace("aricode",quietly = TRUE)){
      ami=aricode::AMI(Cls1, Cls2)
      
      return(ami)
    }else{
      warning("ClusterAMI:: Subordinate  package (aricode) is missing.
                  Please install the package which is defined in 'Suggests'.")
      return(
        "Subordinate  package (aricode) is missing.
                  Please install the package which is defined in 'Suggests'."
      )
      
      
    }
  
}