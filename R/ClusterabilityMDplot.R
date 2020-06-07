ClusterabilityMDplot=function(DataOrDistance,Method="pca",na.rm=FALSE,...){
  #
  # INPUT
  # DataOrDistances[1:n,1:d]    Dataset with n observations and d features or distance matrix with size n
  #
  # OPTIONAL
  # Method                      Choose option: "none" performs no dimension reductio
  #                                            "pca" uses the scores from the first principal component.
  #                                            "distance" computes pairwise distances (using distance_metric as the metric).
  # na.rm                       statistical testing will not work with missing values, if TRUE values are imputed with averages
  #
  # Author: MT
  requireNamespace('clusterability')
  requireNamespace('ggplot2')
  requireNamespace('signal')
  requireNamespace('reshape2')
  
  dots=list(...)
  
  if (!requireNamespace('clusterability')) {
    message(
      'Subordinate clustering package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(DataOrDistance)),
        Object = "Subordinate clustering package is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  if(is.null(dots[["center"]]))
    center=FALSE
  else
    center=dots$center
  
  if(is.null(dots[["scale"]]))
    scale=FALSE
  else
    scale=dots$scale
  
  if(is.null(dots[["Ordering"]]))
    Ordering="Columnwise"
  else
    Ordering=dots$Ordering
  
  
  #requireNamespace('DataVisualizations')

  ## real code
  if(!is.list(DataOrDistance)){
    if(is.null(dots[["main"]]))
      main=paste('MDplot of Clusterability')
    else
      main=dots$main
    
    if(isFALSE(na.rm)){
      if(sum(!is.finite(DataOrDistance))>0){
        stop('ClusterabilityMDplot: Non-Finite Data found. Please perform imputation before using function because statistical testing will not work otherwise.')
      }
    }
    IsDistance=FALSE
    if(isSymmetric(unname(DataOrDistance))){
      Method="none"
      IsDistance=TRUE
      message("Distance detected, Method is set to 'none'")
      if(isTRUE(na.rm)){
        warning('ClusterabilityMDplot: Imputation of non-finite distances is not available.')
      }
    }else{
      if(isTRUE(na.rm)){
        message('ClusterabilityMDplot: Imputation per mean per cluster is performed. This is experimental.')
        DataOrDistance=apply(DataOrDistance,2,function(x){
          bb=!is.finite(x)
          if(sum(bb)<length(x))
            x[bb]=mean(x[!bb],na.rm = T)
          else
            x[bb]=0
        
          return(x)
        })
      }
    }
  pvalm=clusterability::clusterabilitytest(DataOrDistance,reduction = Method,test = 'dip',pca_scale=scale,pca_center=scale,is_dist_matrix = IsDistance)
  #print(pvalm$pvalue)
  pvalue=round(pvalm$pvalue,2)
  if(pvalue==0) 
    pvalue='p < 0.01'
  else
    pvalue=paste('p =',pvalue)
  

  if(isFALSE(isSymmetric(unname(DataOrDistance)))){
    
    if(Method!="distance"){
      res <- prcomp(x=DataOrDistance,retx=T,scale. =scale,tol = 0,center=center)
      TransData=as.matrix(res$x)
      ProjectedPoints=TransData[,1]
    }else{
      x=as.matrix(dist(DataOrDistance))
      ProjectedPoints=x[upper.tri(x,diag = FALSE)]
    }
  
    if(requireNamespace("DataVisualizations")){
      plot=DataVisualizations::MDplot(as.vector(ProjectedPoints),Names = pvalue,Ordering = Ordering,OnlyPlotOutput = TRUE)+ggplot2::ggtitle(main)+
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
        ggplot2::xlab('Probability that data has no cluster structure')+
        ggplot2::ylab('PDE of 1st principal component')
    }
    else{
      stop('DataVisualizations package not loaded or installed.')
    }
    
  }else{
    x=DataOrDistance[upper.tri(DataOrDistance,diag = FALSE)]
    
    if(requireNamespace("DataVisualizations")){
      plot=DataVisualizations::MDplot(x,Names = pvalue,Ordering = Ordering,OnlyPlotOutput = TRUE)+
        ggplot2::ggtitle(main)+
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
        ggplot2::xlab('Probability that data has no cluster structure')+
        ggplot2::ylab('PDE of distance distribution')
    }
    else{
      stop('DataVisualizations package not loaded or installed.')
    }
  }
  
  }else{#dataordistance is list
    n=length(DataOrDistance)
    isDistance=unlist(lapply(DataOrDistance, function(x) isSymmetric(unname(x))))
    
    pvalsL=lapply(DataOrDistance, function(x,Method,na.rm,center,scale){
      IsDistance_hlp=FALSE
      if(isSymmetric(unname(x))){
        Method="none"
        IsDistance_hlp=TRUE
        if(isTRUE(na.rm)){
          warning('ClusterabilityMDplot: Imputation of non-finite distances is not available.')
        }
      }else{
        if(isTRUE(na.rm)){
          x=apply(x,2,function(x2){
            bb=!is.finite(x2)
            if(sum(bb)<length(x2))
              x2[bb]=mean(x2[!bb],na.rm = T)
            else
              x2[bb]=0
            
            return(x2)
          })
        }
      }
      return(clusterability::clusterabilitytest(x,reduction = Method,test = 'dip',pca_scale=scale,pca_center=center,is_dist_matrix = IsDistance_hlp)$pvalue)
    },Method,na.rm,center,scale) 
    Names=names(DataOrDistance)
    vals=unlist(pvalsL)
    vals=round(vals,2)
    ind=which(vals==0)
     ind2=which(vals!=0)
     vals[ind]='p < 0.01'
     vals[ind2]=paste("p =",vals[ind2])
	 # Modes depricated
    if(is.null(Names)){
      #Ordering = 'Columnwise'
      Names=as.character(vals)
    }else{
      Names=paste0(Names,', ',vals)
      #if(Method!="distance")
      #  Ordering = 'Bimodal'
     # else
      #  Ordering = 'Columnwise'
    }
    
    pcasordistances=lapply(DataOrDistance, function(x,Method,scale,center){
      
      if(isFALSE(isSymmetric(unname(x)))){
        if(Method!="distance"){
          res <- prcomp(x=x,retx=T,scale. =scale,tol = 0,center=center)
          TransData=as.matrix(res$x)
          ProjectedPoints=as.vector(TransData[,1])
        }else{
          x=as.matrix(dist(x))
          ProjectedPoints=as.vector(x[upper.tri(x,diag = FALSE)])
        }
        return(ProjectedPoints)
      }else{
        return(x[upper.tri(x,diag = FALSE)])
      }
    },Method,scale,center)
    
    if(is.null(dots[["main"]]))
      main=paste('MDplot of clusterability for multiple datasets')
    else
      main=dots$main
    
    names(pcasordistances)=Names
    
    if(requireNamespace("DataVisualizations")){
      plot=DataVisualizations::MDplot4multiplevectors(pcasordistances,Gaussian_lwd=0.5,Names = Names,Ordering = Ordering,Scaling = 'Robust')+
        ggplot2::ggtitle(main)+
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
        ggplot2::xlab('Probability that data has no cluster structure')
    }
    else{
      stop('DataVisualizations package not loaded or installed.')
    }
    
    if(sum(isDistance)==0){
      plot+ggplot2::ylab('PDE of 1st principal component')
    }else{
      plot+ ggplot2::ylab('PDE of 1st principal component/distance distribution')
    }
  }#end dataordistance is list
  return(plot)
}
## internal functions ----

stat_pde_density <- function(mapping = NULL,
                             data = NULL,
                             geom = "violin",
                             position = "dodge",
                             ...,
                             trim = TRUE, #enden des violins werden korrekt angezeigt und nicht ueber den wertebereich fortgesetzt
                             scale = "area",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  
  scale <- match.arg(scale, c("area", "count", "width"))
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatPDEdensity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      na.rm = na.rm,
      ...
    )
  )
}


compute_pdedensity <- function(x) {
  nx <- length(x)
  
  # If less than 2 points, then return data frame of NAs and a warning
  if (nx < 2) {
    warning("stat_pde_density: Groups with fewer than two data points have been dropped.",
            call. = FALSE)
    return(
      data.frame(
        x = NA_real_,
        density = NA_real_,
        scaled = NA_real_,
        count = NA_real_,
        n = NA_integer_
      )
    )
  }
  
  ##MT: chatch error of one unique value
  Flag <- FALSE
  if (length(unique(x)) ==1) {
    warning('stat_pde_density: Only one unique value in data.')
    if(unique(x)!=0)
      x <- c(unique(x), head(x, 1) * runif(1, 0.999, 1.001))
    else
      x <- c(unique(x), head(x, 1) + runif(1, 0.999, 1.001))
    
    Flag <- TRUE
  }
  requireNamespace('DataVisualizations')
  dens <- DataVisualizations::ParetoDensityEstimation(Data = x)
  
  # Density cannot be estimated, set density to value 1
  if (Flag) {
    # Scatter kernels a little to visualize several features if given
    dens$kernels <- dens$kernels * runif(length(dens$kernels), 0.998, 1.002)
    x <- max(dens$kernels) - min(dens$kernels)
    dens$paretoDensity[1:length(dens$paretoDensity)] <- 1 / x # integral over pdf should be 1
  }
  data.frame(
    x = dens$kernels,
    density = dens$paretoDensity,
    scaled =  dens$paretoDensity / max(dens$paretoDensity, na.rm = TRUE),
    count =   dens$paretoDensity * nx,
    n = nx
  )
  
}

StatPDEdensity <- ggproto("StatPDEdensity",
                          Stat,
                          required_aes = c("x", "y"),
                          
                          compute_group = function(data,
                                                   scales,
                                                   width = NULL,
                                                   trim = TRUE,
                                                   na.rm = FALSE) {
                            if (nrow(data) < 3)
                              return(data.frame())
                            range <- range(data$y, na.rm = TRUE)
                            modifier <- if (trim) 0 else 3
                            dens <- compute_pdedensity(data$y)
                            
                            dens$y <- dens$x
                            dens$x <- mean(range(data$x))
                            
                            # Compute width if x has multiple values
                            if (length(unique(data$x)) > 1) {
                              width <- diff(range(data$x)) * 0.9
                            }
                            dens$width <- width
                            
                            dens
                          },
                          
                          compute_panel = function(self,
                                                   data,
                                                   scales,
                                                   width = NULL,
                                                   trim = TRUE,
                                                   na.rm = FALSE,
                                                   scale = "area") {
                            data <- ggproto_parent(Stat, self)$compute_panel(
                              data,
                              scales,
                              width = width,
                              trim = trim,
                              na.rm = na.rm
                            )
                            
                            # Choose how violins are scaled relative to each other
                            data$violinwidth <- switch(
                              scale,
                              # Area : keep the original densities but scale them to a max width of 1
                              #        for plotting purposes only
                              area = data$density / max(data$density),
                              # Count: use the original densities scaled to a maximum of 1 (as above)
                              #        and then scale them according to the number of observations
                              count = data$density / max(data$density) * data$n / max(data$n),
                              # Width: constant width (density scaled to a maximum of 1)
                              width = data$scaled
                            )
                            data
                          }
                          
)
