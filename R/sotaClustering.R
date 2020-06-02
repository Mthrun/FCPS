SOTAclustering=sotaClustering =function(Data,ClusterNo,PlotIt=FALSE,UnrestGrowth,...){
  # Cls=sotaClustering(Data,ClusterNo=2)
  # Self-organizing Tree Algorithm (SOTA)
  #
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  # ClusterNo         Number of clusters to search for
  #
  # OUTPUT
  # 
  #
  # Author: Luis Winckelmann
  
  if(missing(UnrestGrowth)){
  UnrestGrowth=TRUE
  }
  if(isFALSE(UnrestGrowth))#maxCycles is number of iterations
	res=sota(Data,maxCycles = ClusterNo,unrest.growth=UnrestGrowth,...)
  else #maxCycles is number of of clusters+1
	res=sota(Data,maxCycles = ClusterNo-1,	unrest.growth=UnrestGrowth,...)
	
  Cls=res$clust
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
    
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,sotaObject=res))
} #end of SOTAclustering


# Helper functions needed for sota Clustering, copied from clValid cause it's ORPHANED

sota <- function (data, maxCycles, maxEpochs = 1000, distance = "euclidean", 
                  wcell = 0.01, pcell = 0.005, scell = 0.001, delta = 1e-04, 
                  neighb.level = 0, maxDiversity = 0.9, unrest.growth = TRUE, 
                  ...) 
{
  tree <- sota.init(data)
  pr <- 4:ncol(tree)
  n <- 3
  genes <- 1:nrow(data)
  clust <- rep(1, length(genes))
  Node.Split <- 1
  Res.V <- getResource(data, tree, clust, distance, pr)
  if (distance == "correlation") 
    Res.V <- 1 - Res.V
  diversity <- Res.V
  for (k in 1:maxCycles) {
    trainNode <- Node.Split
    trainSamp <- genes[clust == trainNode]
    curr.err <- 1e+10
    ep <- 1
    while (ep <= maxEpochs) {
      last.err <- 0
      left.ctr <- right.ctr <- 0
      left.d <- right.d <- 0
      for (i in trainSamp) {
        cells <- tree[c(n - 1, n), ]
        dist <- rep(0, nrow(cells))
        for (j in 1:2) dist[j] <- dist.fn(data[i, ], 
                                          cells[j, pr], distance = distance)
        or <- which.min(dist)
        if (or == 1) 
          left.ctr <- left.ctr + 1
        else right.ctr <- right.ctr + 1
        closest <- cells[or, 1]
        sis <- ifelse(closest%%2 == 0, closest + 1, 
                      closest - 1)
        sis.is.cell <- ifelse(tree[sis, "cell"] == 1, 
                              1, 0)
        if (sis.is.cell == 1) {
          parent <- tree[closest, "anc"]
          tree[closest, pr] <- tree[closest, pr] + wcell * 
            (data[i, ] - tree[closest, pr])
          tree[sis, pr] <- tree[sis, pr] + scell * (data[i, 
                                                         ] - tree[sis, pr])
          tree[parent, pr] <- tree[parent, pr] + pcell * 
            (data[i, ] - tree[parent, pr])
        }
        else {
          tree[closest, pr] <- tree[closest, pr] + wcell * 
            (data[i, ] - tree[closest, pr])
        }
      }
      cells <- tree[c(n - 1, n), ]
      for (i in trainSamp) {
        for (j in 1:2) dist[j] <- dist.fn(data[i, ], 
                                          cells[j, pr], distance = distance)
        last.err <- last.err + min(dist)
      }
      last.err <- last.err/length(trainSamp)
      if (ifelse(last.err == 0, 0, abs((curr.err - last.err)/last.err)) < 
          delta && left.ctr != 0 && right.ctr != 0) 
        break
      ep <- ep + 1
      curr.err <- last.err
    }
    clust <- assignGenes(data, trainSamp, clust, tree, n, 
                         distance, pr, neighb.level)
    Res.V <- getResource(data, tree, clust, distance, pr)
    if (distance == "correlation") 
      Res.V <- 1 - Res.V
    tempRes <- Res.V
    tempRes[tempRes == 0] <- diversity[tempRes == 0]
    diversity <- tempRes
    if (k == maxCycles || (max(Res.V) < maxDiversity & unrest.growth == 
                           FALSE)) 
      break
    newCells <- splitNode(Res.V, tree, n)
    tree <- newCells$tree
    n <- newCells$n
    Node.Split <- newCells$toSplit
  }
  tree <- trainLeaves(data, tree, clust, pr, wcell, distance, 
                      n, delta)
  Res.V <- getResource(data, tree, clust, distance, pr)
  Res.V <- Res.V[Res.V != 0]
  if (distance == "correlation") 
    Res.V <- 1 - Res.V
  diversity[(length(diversity) - length(Res.V) + 1):length(diversity)] <- Res.V
  treel <- tree[tree[, "cell"] == 1, ]
  old.cl <- treel[, 1]
  treel[, 1] <- 1:nrow(treel)
  old.clust <- clust
  clust <- cl.ID(old.clust, old.cl, 1:nrow(treel))
  totals <- table(clust)
  out <- list(data = data, c.tree = cbind(tree[1:n, ], Diversity = diversity), 
              tree = treel, clust = clust, totals = totals, dist = distance, 
              diversity = Res.V)
  class(out) <- "sota"
  return(out)
}

sota.init <- function (data) 
{
  nodes <- matrix(0, nrow(data) * 2, 3 + ncol(data))
  if (is.null(colnames(data))) 
    colnames(data) <- paste("V", 1:ncol(data))
  colnames(nodes) <- c("ID", "anc", "cell", colnames(data))
  nodes[, "ID"] = 1:(nrow(data) * 2)
  nodes[1, ] <- c(1, 0, 0, apply(data, 2, function(x) mean(x, 
                                                           na.rm = TRUE)))
  nodes[2, ] <- c(2, 1, 1, nodes[1, ][-c(1, 2, 3)])
  nodes[3, ] <- c(3, 1, 1, nodes[1, ][-c(1, 2, 3)])
  return(nodes)
}


getResource <- function (data, tree, clust, distance, pr) 
{
  dist <- rep(0, length(clust))
  resource <- rep(0, max(clust))
  for (i in unique(clust)) {
    temp <- data[clust == i, ]
    if (is.vector(temp)) 
      temp <- matrix(temp, nrow = 1, ncol = ncol(data))
    if (distance == "correlation") 
      resource[i] <- mean(apply(temp, 1, dist.fn, profile = tree[i, 
                                                                 pr], distance = distance))
    else resource[i] <- mean(apply(temp, 1, dist.fn, profile = tree[i, 
                                                                    pr], distance = distance))
  }
  resource
}


dist.fn <- function (input, profile, distance) 
{
  if (distance == "correlation") 
    return(1 - cor(input, profile, use = "pairwise.complete.obs"))
  else return(sqrt(sum((input - profile)^2)))
}

assignGenes <- function (data, Sample, clust, tree, n, distance, pr, neighb.level) 
{
  if (neighb.level == 0) 
    cells <- tree[c(n - 1, n), ]
  else cells <- getCells(tree, neighb.level, n)
  for (i in Sample) {
    dist <- rep(0, nrow(cells))
    for (j in 1:nrow(cells)) dist[j] <- dist.fn(data[i, 
                                                     ], cells[j, pr], distance)
    or <- which.min(dist)
    closest <- cells[or, 1]
    clust[i] <- closest
  }
  clust
}

splitNode <- function (Res.V, tree, n) 
{
  maxheter <- which.max(Res.V)
  cl.to.split <- tree[maxheter, 1]
  tree[n <- n + 1, -1] <- tree[cl.to.split, -1]
  tree[n, "anc"] <- cl.to.split
  tree[n <- n + 1, -1] <- tree[cl.to.split, -1]
  tree[n, "anc"] <- cl.to.split
  tree[cl.to.split, "cell"] <- 0
  return(list(tree = tree, n = n, toSplit = cl.to.split))
}

trainLeaves <- function (data, tree, clust, pr, wcell, distance, n, delta) 
{
  nc <- ncol(data)
  for (i in 1:n) {
    if (!is.element(i, clust)) 
      next
    temp <- matrix(data[clust == i, ], ncol = nc)
    converged <- FALSE
    init.err <- getCellResource(temp, tree[i, pr], distance)
    while (!converged) {
      for (j in 1:nrow(temp)) tree[i, pr] <- tree[i, pr] + 
          wcell * (temp[j, ] - tree[i, pr])
      last.err <- getCellResource(temp, tree[i, pr], distance)
      converged <- ifelse(abs((last.err - init.err)/last.err) < 
                            delta, TRUE, FALSE)
      init.err <- last.err
    }
  }
  return(tree)
}

cl.ID <- function (clust, old.cl, new.cl) 
{
  for (i in 1:length(clust)) clust[i] <- new.cl[which(old.cl == 
                                                        clust[i])]
  clust
}

getCellResource <- function (temp, profile, distance) 
{
  if (distance == "correlation") 
    resource <- mean(apply(temp, 1, dist.fn, profile, distance = distance))
  else (distance == "euclidean")
  resource <- mean(apply(temp, 1, dist.fn, profile, distance = distance))
  resource
}

getCells <- function(tree, neighb.level, n){
  or.n <- n
  cells <- c(n-1,n)
  for(i in 1:(neighb.level+1)){
    n  <- tree[n, "anc"]
    if(n==1)
      break
  }
  for(j in 2:(or.n-2)){
    z <- j
    if(tree[j,"cell"]!=1)
      next
    while(z > 0){
      z <- tree[z, "anc"]
      if(z==n){
        cells <- c(cells, j)
        break}
    }
  }
  return(tree[cells,])
}
