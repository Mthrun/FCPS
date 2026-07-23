ClusterNMI=function(Cls1,Cls2,Variant="max"){

    if(requireNamespace("aricode",quietly = TRUE)){
      nmi=aricode::NMI(Cls1, Cls2,variant = Variant)
      
      return(nmi)
    }else{
      warning("ClusterNMI:: Subordinate  package (aricode) is missing.
                  Please install the package which is defined in 'Suggests'.")
      return(
        "Subordinate  package (aricode) is missing.
                  Please install the package which is defined in 'Suggests'."
      )
      
      
    }
  
}