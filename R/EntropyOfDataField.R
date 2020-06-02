EntropyOfDataField=function(Data,sigmarange=c(0.01,0.1,0.5,1,2,5,8,10,100),PlotIt=TRUE){
  # EntropyOfDataField(Data, sigmarange = c(0.01, 0.1, 0.5, 1, 2, 5, 8, 10, 100), PlotIt = TRUE)
  # Entropy Of a Data Field [Wang et al., 2011].
  # Calculates the Potential Entropy Of a Data Field for a givven ranges of impact factors sigma
  # 
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # 
  # OPTIONAL
  # sigmarange    numeric vector [1:s] of relevant sigmas
  # PlotIt        FALSE: disable plot, TRUE: Plot with upper boundary of H after [Wang et al., 2011].
  #   
  # In theory there should be a courve with a clear minimum of Entropy [Wang et al.,2011]. Then the choice for the impact factor sigma is the minimum of the entropy to defined the correct data field. It follows, that the influence radius is 3/sqrt(2)*sigma (3B rule of gaussian distribution) for clustering algorithms like Density Peak clustering [Wang et al.,2011].
  #
  # OUTPUT
  # [1:s] named vector of the Entropy of data field. The names are the impact factor sigma
  # 
  # 
  # [Wang et al., 2015] Wang, S., Wang, D., Li, C., & Li, Y.: Comment on" Clustering by fast search and find of density peaks", arXiv preprint arXiv:1501.04267, 2015.
  #   
  # [Wang et al., 2011]  Wang, S., Gan, W., Li, D., & Li, D.: Data field for hierarchical clustering, International Journal of Data Warehousing and Mining (IJDWM), Vol. 7(4), pp. 43-63. 2011.
  #   
  # Author: Michael Thrun
  # 
  
  DistanceFull=as.matrix((parallelDist::parDist(Data)))
  H=c()
  k=1

  for(sigma in sigmarange){
    # phi=unlist(lapply(NormList,
    #                   function(x,sigma) return(sum(exp(-(x/sigma)),na.rm=T))
    #                   ,sigma)
    #            )
    # phi[phi<Epsilon^2]=Epsilon^2
    # norm=sum(phi,na.rm=T)
    # 
    
    Dexp=exp(-(DistanceFull/sigma)^2)
    #diag(Dexp)=NaN #punkt zu sich selber darf nicht geloeascht werden, sonst funktioneirt es nicht
    phi=apply(Dexp, 1, sum,na.rm=T) #von jedem punkt zu allen anderen und dann summiert
    norm=sum(phi,na.rm=T)
     
    # if(norm<Epsilon)
    #   phinorm=phi/Epsilon
    # else
       phinorm=phi/sum(phi,na.rm=T)
    #print(phinorm)
    H[k]=-sum(phinorm*log(phinorm))
    k=k+1
    
  }

  if(PlotIt){
    # defined in Wang et al 2011
    # plot(sigmarange,H,ylab='Potential Entropy H',
    #      xlab = 'Points of  Selected Impact Factor Sigma in black, Red: Upper Boundary of H',main='Entropy of Data Field')
    # abline(h = log(nrow(Data)),col='red')
    # 
    requireNamespace('plotly')
    
    p <- plotly::plot_ly( x = ~sigmarange, y = ~H,type = "scatter",mode="markers")
    
    line <- list(
      type = "line",
      line = list(color = "red"),
      xref = "x",
      yref = "y"
    )
    line[["x0"]] <- min(sigmarange)
    line[["x1"]] <- max(sigmarange)
    line[c("y0", "y1")] <- log(nrow(Data))
    
    p=plotly::layout(p,title = "Entropy of data field",
                     xaxis=list(exponentformat = "E",  title = "Points of selected impact factor sigma in black, Red: Upper boundary of H"),
                     yaxis=list(exponentformat = "E",  title = "Potential entropy H"),
                     showlegend = FALSE,shapes=line)
    print(p)
    
  }
  names(H)=sigmarange
  return(H)
}
