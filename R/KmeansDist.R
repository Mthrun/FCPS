kstepdist <- function(centers, dists){
  dists2centers = dists[centers,]
  idclusts = apply(dists2centers, 2, which.min)
  
  for(i in 1:size(centers)[1]){ # Für jedes Cluster
    # Welche Punkte liegen im Cluster
    inClust = which(idclusts == i)
    # Betrachte nur distanzen innerhalb des Clusters
    inClustDist = dists[inClust,inClust]
    # means!
    centers[i] = inClust[which.min(rowMeans(inClustDist))]
  }
  
  return(list(centerids=centers, nClusts=idclusts))
}

kmeansdist <- function(centers, distances, maxIt = 2000, verbose = F){
  stop('kmeansdist function does not work yet.')
  # KMeans version using ONLY datapoints as centers
  # INPUT
  # centers:  Vektor der die ids der initialen centers enthält. Alle centers müssen daher Datenpunkte sein
  #				ODER: Integer, der angibt, wieviele Centroide es geben soll. Centroide werden zufällig aus Daten gewählt
  # distances:Distanzmatrix für den zu clusternden Datensatz.
  #
  # OPTIONAL
  # maxIt:    Maximale Anzahl an Iterationen
  # verbose:  Erhöht Textuelle (Debug-)Ausgaben
  #
  # OUTPUT
  # nmu       k-elementiger Vektor mit den k-Means der Ausgabe
  # nClusts   n elementiger Vektor der eine Clusterzuordnung von jeder Zeile des Datensatzes entspricht
  
  
  if(size(centers)[2] > 1){
    warning("centerids is expected to be vector of indicies")
    return
  }
  if(size(centers)[1] == 1){
    centerids = sample(dim(distances)[1],centers)
  } else {
    centerids = centers
  }
  converged = F
  clusts = NULL
  nClusts = NULL
  i = 0
  while(!converged){
    i <- i+1
	if(i > maxIt){
      converged = T
      print("Warning, did not converge")
    }
	
    if(verbose)
      print(paste('Starting Iteration', toString(i)))

    if(!is.null(clusts) && all(nClusts==clusts)){
      converged <- T
    } else {
      clusts <- nClusts
    }
    step = ksteDistanceMatrix(centerids, distances)
    centerids = step$centerids
    nClusts = step$nClusts
  }
  return(list(centerids=centerids,Cls=nClusts))
}
