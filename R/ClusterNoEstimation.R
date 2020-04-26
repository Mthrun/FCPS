ClusterNoEstimation <- function (DataOrDistances,
                            ClsMatrix = NULL,
                            max.nc,
                            index = 'all',
                            min.nc = 2,
                            Silent = TRUE,
                            method = NULL,
                            PlotIt=TRUE,
                            SelectByABC=TRUE) {

  # berechnet die Kennzahlen zu den gegebenen Daten und Clusterungen und die darauf basierende empfohlene Klassenanzahl
  #
  #  INPUT
  #  DataOrDistances        Daten
  #  ClsMatrix         Clusterungen der zu ueberpruefenden Klassenanzahlen als Matrix mit einer Clusterung pro Spalte
  #              (siehe auch Notes (1) und (2)), muss angegeben werden, wenn method = NULL
  #  max.nc      hoechste Klassenanzahl, die ueberprueft werden soll
  #  method      Clusterverfahren, mit dem die Clusterungen erstellt werden (siehe DETAILS fuer moegliche Methoden),
  #              muss angegeben werden, wenn cls = NULL
  #
  #  OPTIONAL
  #  index           Vektor der Kennzahlen die berechnet werden sollen, Standard = 'all',
  #                  siehe DETAILS fuer moegliche Kennzahlen
  #  min.nc          niedrigste Klassenanzahl, die ueberprueft werden soll, Standard = 2
  #  Silent    wenn TRUE werden Statusmeldungen ausgeben, Standard = FALSE
  #
  #  RETURN
  #  Kennzahlen          Matrix der berechneten Kennzahlen
  #  Klassenanzahl       fuer jede berechnete Kennzahl die empfohlene Klassenanzahl
  #  Clusterungen        die eingebenen Clusterungen
  #  criticalValues      die kritschen Werte fuer die Kennzahlen duda, pseudot2, beale
  #
  #  DETAILS
  #  Es koennen folgende 26 Kennzahlen berechnet werden:
  #
  #  "calinski", "cindex", "db", "hartigan",
  #  "ratkowsky", "scott", "marriot", "ball", "trcovw", "tracew",
  #  "friedman", "rubin", "ssi", "xuindex", "kl", "ccc", "silhouette",
  #  "duda", "pseudot2", "beale", "ptbiserial", "frey",
  #  "mcclain", "dunn", "sdindex", "sdbw"
  #
  #  Diese koennen ueber den Parameter index einzeln oder als Vektor angegeben werden.
  #  Bei Eingabe 'all' werden alle Kennzahlen berechnet.
  #
  #  Zur Erstellung der Clusterungen koennen folgende Methoden verwendet werden:
  #
  #  "ward.D", "single", "complete", "average", "mcquitty", 
  #  "median", "centroid", "ward.D2", "kmeans", "DBSclustering"
  #
  #  NOTES
  #  (1) Die Kennzahlen kl, duda, pseudot2, beale, frey und mcclain benoetigen eine Clusterung
  #  fuer max.nc+1 Klassen. Sollen diese Kennzahlen berechnet werden, muss diese Clusterung in cls
  #  mitangegeben werden.
  #
  #  (2) Die Kennzahl kl benoetigt eine Clusterung fuer min.nc-1 Klassen. Soll diese Kennzahl berechnet werden,
  #  muss dieser Clusterung in cls mitangegeben werden. Fuer den Fall min.nc = 2 muss keine Clusterung fuer 1
  #  angegeben werden.
  #
  #  (3) Die Kennzahlen duda, pseudot2, beale und frey sind nur fuer die Anwendung bei hierarchischen
  #  Clusterverfahren gedacht.
  #
  #  AUTHOR
  #  Peter Nahrgang
  #  1.Edditor: MT: added distances, try() for indicator that not work always, further error catching, fan plotting
  #
  #  REFERENCES
  #  Charrad, Malika, et al. "Package 'NbClust'." J. Stat. Soft 61 (2014): 1-36.
  #  Dimtriadou, E. "cclust: Convex Clustering Methods and Clustering Indexes." R package version 0.6-16, URL http://CRAN. R-project. org/package= cclust (2009).
  cls=ClsMatrix
  if(!is.null(cls)){
    if(!is.matrix(cls)){
      warning('ClsMatrix is not a matrix. calling as.matrix')
      cls=as.matrix(cls)
    }
    if(any(apply(cls,2,function(x) length(unique(x)))<2)){
      stop('Amount of unqiue clusters for each column of ClsMatrix should be at least 2,')
    }
  }							
  

  if (isSymmetric(unname(DataOrDistances))) {
    data=internalMDSestimate(DataOrDistances)
  }else{
    data=DataOrDistances
  }	
  
  range <- max.nc - min.nc + 1
  
  alphabeale <- 0.1
  
  indexnames <- c(
    "calinski",
    "cindex",
    "db",
    "hartigan",
    "ratkowsky",
    "scott",
    "marriot",
    "ball",
    "trcovw",
    "tracew",
    "friedman",
    "rubin",
    "ssi",
    "xuindex",
    "kl",
    "ccc",
    "silhouette",
    "duda",
    "pseudot2",
    "beale",
    "ptbiserial",
    "frey",
    "mcclain",
    "dunn",
    "sdindex",
    "sdbw"
  )
  
  indexanzahl <- length(indexnames)
  all <- indexanzahl + 1
  
  indexn <- pmatch(index, c(indexnames,
                            "all"))
  
  crits <- c()
  if (any(indexn == 18)) {
    crits <- c(1)
  }
  if (any(indexn == 19)) {
    crits <- c(crits, 2)
  }
  if (any(indexn == 20)) {
    crits <- c(crits, 3)
  }
  if (any(indexn == all)) {
    crits <- c(1:3)
  }
  
  #Hilfsfunktionen
  centers <- function(cls) {
    n <- length(cls)
    k <- max(cls)
    
    centers <- matrix(nrow = k, ncol = ncol(data))
    
    for (i in 1:k) {
      for (j in 1:ncol(data)) {
        centers[i, j] <- mean(data[cls == i, j])
      }
    }
    
    return(centers)
  }
  
  withinss <- function(centers, cls) {
    res <- rep(0, nrow(centers))
    
    data <- (data - centers[cls, ]) ^ 2
    
    for (k in 1:nrow(centers)) {
      res[k] <- sum(data[cls == k, ])
    }
    
    return(res)
  }
  varwithinss <- function(x, centers, cluster) {
    nrow <- dim(centers)[1]
    nvar <- dim(x)[2]
    varwithins <- matrix(0, nrow, nvar)
    x <- (x - centers[cluster, ]) ^ 2
    for (l in 1:nvar) {
      for (k in 1:nrow) {
        varwithins[k, l] <- sum(x[cluster == k, l])
      }
    }
    return(varwithins)
  }
  maxmindist <- function(clsize, distscen) {
    ncl <- length(clsize)
    npairs <- 0
    for (i in 1:ncl)
      npairs <- npairs + clsize[i] * (clsize[i] -
                                        1) / 2
    mindw <- 0
    nfound <- distscen[1]
    i <- 1
    while (nfound < npairs) {
      if ((nfound + distscen[i + 1]) < npairs) {
        mindw <- mindw + i * distscen[i + 1]
        nfound <- nfound + distscen[i + 1]
      }
      else {
        mindw <- mindw + i * (npairs - nfound)
        nfound <- nfound + distscen[i + 1]
      }
      i <- i + 1
    }
    maxdw <- 0
    nfound <- 0
    i <- length(distscen) - 1
    while (nfound < npairs) {
      if ((nfound + distscen[i + 1]) < npairs) {
        maxdw <- maxdw + i * distscen[i + 1]
        nfound <- nfound + distscen[i + 1]
      }
      else {
        maxdw <- maxdw + i * (npairs - nfound)
        nfound <- nfound + distscen[i + 1]
      }
      i <- i - 1
    }
    minmaxd <- list(mindw = mindw, maxdw = maxdw)
    return(minmaxd)
  }
  gss <- function(x, clsize, withins) {
    n <- sum(clsize)
    k <- length(clsize)
    allmean <- apply(x, 2, mean)
    dmean <- sweep(x, 2, allmean, "-")
    allmeandist <- sum(dmean ^ 2)
    wgss <- sum(withins)
    bgss <- allmeandist - wgss
    zgss <- list(wgss = wgss, bgss = bgss)
    return(zgss)
  }
  vargss <- function(x, clsize, varwithins) {
    nvar <- dim(x)[2]
    n <- sum(clsize)
    k <- length(clsize)
    varallmean <- rep(0, nvar)
    varallmeandist <- rep(0, nvar)
    varwgss <- rep(0, nvar)
    for (l in 1:nvar)
      varallmean[l] <- mean(x[, l])
    vardmean <- sweep(x, 2, varallmean, "-")
    for (l in 1:nvar) {
      varallmeandist[l] <- sum((vardmean[, l]) ^ 2)
      varwgss[l] <- sum(varwithins[, l])
    }
    varbgss <- varallmeandist - varwgss
    vartss <- varbgss + varwgss
    zvargss <- list(vartss = vartss, varbgss = varbgss)
    return(zvargss)
  }
  
  count <- function(y) {
    x <- trunc(y)
    
    xrows <- nrow(y)
    xcols <- ncol(y)
    d <- xcols + 1
    
    res <- rep(0, d)
    
    for (i in 1:(xrows - 1)) {
      tempi <- x[i,]
      for (j in (i + 1):xrows) {
        c <- 1
        tempj <- x[j,]
        for (k in 1:xcols) {
          test <- tempi[k] != tempj[k]
          if (test) {
            c <- c + 1
          }
        }
        res[c] <- res[c] + 1
      }
    }
    
    res
    
  }
  
  ttww <- function(x, clsize, cluster) {

  
    n <- sum(clsize)
    k <- length(clsize)
    w <- 0
    tt <- cov(x) * n
    zttw <- list(tt =  tt, w = w)
    for (l in 1:k) {
      wtemp=cov(x[cluster == l, ,drop=FALSE]) * clsize[l]
      if(sum(is.na(wtemp))==0)
        w <- w + wtemp
    }
    zttw$w=w
 
    return(zttw)
  }
  
  Average.scattering <- function (cl, x)
  {
    x <- as.matrix(x)
    n <- length(cl)
    k <- max(cl)
    centers.matrix <- centers(cl)
    
    cluster.size <- numeric(0)
    variance.clusters <- matrix(0, ncol = ncol(x), nrow = k)
    var <- matrix(0, ncol = ncol(x), nrow = k)
    
    for (u in 1:k)
      cluster.size[u] <- sum(cl == u)
    
    for (u in 1:k)
    {
      for (j in 1:ncol(x))
      {
        for (i in 1:n)
        {
          if (cl[i] == u)
            variance.clusters[u, j] <-
              variance.clusters[u, j] + (x[i, j] - centers.matrix[u, j]) ^ 2
        }
      }
    }
    
    for (u in 1:k)
    {
      for (j in 1:ncol(x))
        variance.clusters[u, j] = variance.clusters[u, j] / cluster.size[u]
    }
    
    
    variance.matrix <- numeric(0)
    for (j in 1:ncol(x))
      variance.matrix[j] = var(x[, j]) * (n - 1) / n
    
    
    Somme.variance.clusters <- 0
    for (u in 1:k)
      Somme.variance.clusters <-
      Somme.variance.clusters + sqrt((variance.clusters[u, ] %*% (variance.clusters[u, ])))
    
    
    # Standard deviation
    stdev <- (1 / k) * sqrt(Somme.variance.clusters)
    
    #Average scattering for clusters
    scat <-
      (1 / k) * (Somme.variance.clusters / sqrt(variance.matrix %*% variance.matrix))
    
    scat <-
      list(
        stdev = stdev,
        centers = centers.matrix,
        variance.intraclusters = variance.clusters,
        scatt =
          scat
      )
    return(scat)
  }
  
  density.clusters <- function(cl, x)
  {
    x <- as.matrix(x)
    k <- max(cl)
    n <- length(cl)
    
    distance <- matrix(0, ncol = 1, nrow = n)
    density <-  matrix(0, ncol = 1, nrow = k)
    centers.matrix <- centers(cl)
    stdev <- Average.scattering(cl, x)$stdev
    for (i in 1:n)
    {
      u = 1
      while (cl[i] != u)
        u <- u + 1
      for (j in 1:ncol(x))
      {
        distance[i] <- distance[i] + (x[i, j] - centers.matrix[u, j]) ^ 2
      }
      distance[i] <- sqrt(distance[i])
      if (distance[i] <= stdev)
        density[u] = density[u] + 1
    }
    dens <- list(distance = distance, density = density)
    return(dens)
    
  }
  
  
  density.bw <- function(cl, x)
  {
    x <- as.matrix(x)
    k <- max(cl)
    n <- length(cl)
    centers.matrix <- centers(cl)
    stdev <- Average.scattering(cl, x)$stdev
    density.bw <- matrix(0, ncol = k, nrow = k)
    u <- 1
    
    for (u in 1:k)
    {
      for (v in 1:k)
      {
        if (v != u)
        {
          distance <- matrix(0, ncol = 1, nrow = n)
          moy <- (centers.matrix[u, ] + centers.matrix[v, ]) / 2
          for (i in 1:n)
          {
            if ((cl[i] == u) || (cl[i] == v))
            {
              for (j in 1:ncol(x))
              {
                distance[i] <- distance[i] + (x[i, j] - moy[j]) ^ 2
              }
              distance[i] <- sqrt(distance[i])
              if (distance[i] <= stdev)
              {
                density.bw[u, v] <- density.bw[u, v] + 1
              }
            }
          }
        }
      }
    }
    density.clust <- density.clusters(cl, x)$density
    S <- 0
    for (u in 1:k)
      for (v in 1:k)
      {
        if (max(density.clust[u], density.clust[v]) != 0)
          S = S + (density.bw[u, v] / max(density.clust[u], density.clust[v]))
      }
    density.bw <- S / (k * (k - 1))
    return(density.bw)
    
  }
  
  
  
  Dis <- function (cl, x)
  {
    # Dis : Total separation between clusters
    
    x <- as.matrix(x)
    k <- max(cl)
    centers.matrix <- centers(cl)
    Distance.centers <- dist(centers.matrix)
    Dmin <- min(Distance.centers)
    Dmax <- max(Distance.centers)
    Distance.centers <- as.matrix(Distance.centers)
    s2 <- 0
    for (u in 1:k)
    {
      s1 = 0
      for (j in 1:ncol(Distance.centers))
      {
        s1 <- s1 + Distance.centers[u, j]
      }
      s2 <- s2 + 1 / s1
    }
    Dis <- (Dmax / Dmin) * s2
    return(Dis)
  }
  #Hilfsfunktionen ende
  ########
  
  # verarbeitung der clss
  
  if (is.null(method)) {
  
  if (any(indexn == 15,
          indexn == 18,
          indexn == 19,
          indexn == 20,
          indexn == 22,
          indexn == 23,
          indexn == 27)) {
    if (dim(cls)[2] == range) {
      stop("Columns of ClsMatrix are expexted to be from min.nc to max.nc. However to number of columns does not equal the range of cluster numbers to be investigated. Please provide appropriate choice for max.nc and min.nc. ")
    }
    else if (dim(cls)[2] == range + 1 &&
             any(indexn == 15, indexn == 27) && min.nc != 2) {
      stop("Selected indicators requaire min.nc to be set with two.")
    }
    else if (dim(cls)[2] == range + 1) {
      clusters <- cls[, 1:range]
      colnames(clusters) <- c(min.nc:max.nc)
      clusters2 <- cls
      clusters2 <- cbind(rep(1, dim(cls)[1]), cls)
      colnames(clusters2) <- c(1, min.nc:(max.nc + 1))
    }
    else if (dim(cls)[2] == range + 2) {
      clusters <- cls[, 2:(range + 1)]
      colnames(clusters) <- c(min.nc:max.nc)
      clusters2 <- cls
      colnames(clusters2) <- c((min.nc - 1):(max.nc + 1))
    }
  }
  else {
    clusters <- cls
    colnames(clusters) <- c(min.nc:max.nc)
    clusters2 <-
      cbind(rep(NA, dim(cls)[1]), cls, rep(NA, dim(cls)[1]))
    colnames(clusters2) <- c(NA, min.nc:max.nc, NA)
  }
  
  if (!Silent) {
   print("eingebene Clusterungen in Ordnung, starte Berechnung") 
  }
  }
  else {
    if (!Silent) {
      print("Clusterungen werden erstellt") 
    }
    methodnames <- c("ward.D", "single", "complete", "average", "mcquitty", 
                     "median", "centroid", "ward.D2","kmeans","DBSclustering")
    methodn <- pmatch(method,methodnames)
    clusters2 <- matrix(1,nrow = dim(data)[1],ncol = range + 2)
    if (methodn > 0 && methodn <= 8) {
      md <- dist(data)
      hc <- hclust(md,method)
      for (i in 0:(range + 1)) {
        if (i != 0 || !min.nc == 2) {
          temp <- cutree(hc,min.nc-1+i)
          clusters2[,i+1] <- temp
        }
      }
      
    }
    else if (methodn == 9) {
      for (i in 0:(range + 1)) {
        if (i != 0 || !min.nc == 2) {
          temp <- kmeans(data,min.nc-1+i)$cluster
          clusters2[,i+1] <- temp
        }
      }
    }
    else if (methodn == 10) {
	requireNamespace('DatabionicSwarm')
      projPoints <- DatabionicSwarm::Pswarm(data)
      
      for (i in 0:(range + 1)) {
        if (i != 0 || !min.nc == 2) {
          temp <- DatabionicSwarm::DBSclustering(min.nc-1+i,data,projPoints$ProjectedPoints,projPoints$LC[c(2,1)])
          clusters2[,i+1] <- temp
        }
      }
      
      
    }
    else {
      stop("falsche methode")
    }
    
    colnames(clusters2) <- c((min.nc - 1):(max.nc + 1))
    clusters <- clusters2[, 2:(range + 1)]
    colnames(clusters) <- c(min.nc:max.nc)
    
    if (!Silent) {
      print("Clusterungen erstellt, starte Berechnung") 
    }
  }
  
  ######
  
  #Funktionen zur Berechnung der Kennzahlen
  #Kennzahlen aus cclust
  calinski <- function(zgss, clsize) {
    n <- sum(clsize)
    k <- length(clsize)
    vrc <- (zgss$bgss / (k - 1)) / (zgss$wgss / (n - k))
    
    return(vrc = vrc)
  }
  cindex <- function(withins, minmaxd, clsize) {
    dw <- sum(withins * clsize)
    cindex <- (dw - minmaxd$mindw) / (minmaxd$maxdw - minmaxd$mindw)
    return(cindex)
  }
  db <- function(withins, centers, cluster) {
    mse <- withins / table(cluster)
    r <-
      outer(mse, mse, "+") / as.matrix(dist(centers, diag = TRUE))
    diag(r) <- 0
    db <- mean(apply(r, 1, max))
    return(db)
  }
  hartigan <- function(zgss) {
    hart <- log(zgss$bgss / zgss$wgss)
    return(hart)
  }
  ratkowsky <- function(zvargss, clsize) {
    k <- length(clsize)
    ratio <- mean(sqrt(zvargss$varbgss / zvargss$vartss))
    rat <- ratio / sqrt(k)
    return(rat)
  }
  scott <- function(zttw, clsize) {
    scott=NaN
    try({
    n <- sum(clsize)
    dettt <- prod(eigen(zttw$tt)$values)
    detw <- prod(eigen(zttw$w)$values)
    scott <- n * log(dettt / detw)
    })
    return(scott)
  }
  marriot <- function(zttw, clsize) {
    mar=NaN
    try({
    k <- length(clsize)
    detw <- prod(eigen(zttw$w)$values)
    mar <- (k ^ 2) * detw
    })
    return(mar)
  }
  ball <- function(withins, clsize) {
    ball <- sum(withins) / length(clsize)
  }
  tracecovw <- function(zttw) {
    trcovw=NaN
    try({
    trcovw <- sum(diag(cov(zttw$w)))
    })
    return(trcovw)
  }
  tracew <- function(zttw) {
    tracew <- sum(diag(zttw$w))
    return(tracew)
  }
  friedman <- function(zttw) {
    fried=NaN
    try({
    b <- zttw$tt - zttw$w
    fried <- sum(diag(solve(zttw$w) %*% b))
    })
    return(fried)
  }
  rubin <- function(zttw) {
    friedm=NaN
    try({
    dettt <- prod(eigen(zttw$tt)$values)
    detw <- prod(eigen(zttw$w)$values)
    friedm <- dettt / detw
    })
    return(friedm)
  }
  ssi <- function(centers, clsize) {
    ncl <- dim(centers)[1]
    nvar <- dim(centers)[2]
    n <- sum(clsize)
    cmax <- apply(centers, 2, max)
    cmin <- apply(centers, 2, min)
    cord <- apply(centers, 2, order)
    cmaxi <- cord[ncl, ]
    cmini <- cord[1, ]
    meanmean <- mean(centers)
    absmdif <- abs(apply(centers, 2, mean) - meanmean)
    span <- cmax - cmin
    csizemax <- clsize[cmaxi]
    csizemin <- clsize[cmini]
    hiest <- nvar
    hiestw <-
      hiest * max(span) * max(max(csizemax), max(csizemin)) *
      exp(-min(absmdif))
    sist <- sum(span) / hiest
    sistw <-
      (span * exp(-absmdif)) %*% sqrt(csizemax * csizemin) / hiestw
    return(list(ssi = sist, ssiw = sistw))
  }
  xu <- function(x, clsize, zgss) {
    n <- sum(clsize)
    k <- length(clsize)
    d <- dim(x)[2]
    xuindex <- d * log(sqrt(zgss$wgss / (d * (n ^ 2)))) + log(k)
    return(xuindex)
  }
  #cclust ende
  
  #kennzahlen aus nbclust
  ##################################
  #                                #
  #      Frey and McClain          #
  #                                #
  ##################################
  
  
  
  
  Index.15and28  <- function (cl1, cl2, md)
  {
    cn1 <- max(cl1)
    n1 <- length(cl1)
    dmat <- as.matrix(md)
    average.distance <-
      median.distance <-
      separation <-
      cluster.size <- within.dist1 <- between.dist1 <- numeric(0)
    separation.matrix <- matrix(0, ncol = cn1, nrow = cn1)
    di <- list()
    for (u in 1:cn1)
    {
      cluster.size[u] <- sum(cl1 == u)
      du <- as.dist(dmat[cl1 == u, cl1 == u])
      within.dist1 <- c(within.dist1, du)
      #average.distance[u] <- mean(du)
      #median.distance[u] <- median(du)
      #bv <- numeric(0)
      for (v in 1:cn1) {
        if (v != u) {
          suv <- dmat[cl1 == u, cl1 == v]
          #bv <- c(bv, suv)
          if (u < v) {
            separation.matrix[u, v] <- separation.matrix[v, u] <- min(suv)
            between.dist1 <- c(between.dist1, suv)
          }
        }
      }
    }
    cn2 <- max(cl2)
    n2 <- length(cl2)
    dmat <- as.matrix(md)
    average.distance <-
      median.distance <-
      separation <-
      cluster.size <- within.dist2 <- between.dist2 <- numeric(0)
    separation.matrix <- matrix(0, ncol = cn2, nrow = cn2)
    di <- list()
    for (w in 1:cn2) {
      cluster.size[w] <- sum(cl2 == w)
      dw <- as.dist(dmat[cl2 == w, cl2 == w])
      within.dist2 <- c(within.dist2, dw)
      #average.distance[w] <- mean(dw)
      #median.distance[w] <- median(dw)
      bx <- numeric(0)
      for (x in 1:cn2) {
        if (x != w) {
          swx <- dmat[cl2 == w, cl2 == x,drop=FALSE]
          bx <- c(bx, swx)
          if (w < x) {
            separation.matrix[w, x] <- separation.matrix[x, w] <- min(swx)
            between.dist2 <- c(between.dist2, swx)
          }
        }
      }
    }
    nwithin1 <- length(within.dist1)
    nbetween1 <- length(between.dist1)
    meanwithin1 <- mean(within.dist1)
    meanbetween1 <- mean(between.dist1)
    meanwithin2 <- mean(within.dist2)
    meanbetween2 <- mean(between.dist2)
    Index.15 <-
      (meanbetween2 - meanbetween1) / (meanwithin2 - meanwithin1)
    Index.28 <-
      (meanwithin1 / nwithin1) / (meanbetween1 / nbetween1)
    
    results <- list(frey = Index.15, mcclain = Index.28)
    return(results)
  }
  
  
  ##################################
  #                                #
  #      Point-biserial            #
  #                                #
  ##################################
  
  
  
  Indice.ptbiserial <- function (x, md, cl1)
  {
    nn <- dim(x)[1]
    pp <- dim(x)[2]
    
    md2 <- as.matrix(md)
    m01 <- array(NA, c(nn, nn))
    nbr <- (nn * (nn - 1)) / 2
    pby <- rep(0,nbr)
    pbx <- md2[row(md2) < col(md2)]
    
    m3 <- 1
    
    for (m1 in 2:nn)
    {
      m12 <- m1 - 1
      for (m2 in 1:m12)
      {
        if (cl1[m1] == cl1[m2]) {
          pby[m3] <- 0
        }
        else {
          pby[m3] <- 1
        }
        m3 <- m3 + 1
      }
    }
    
    y <- pby
    x <- pbx
    
    biserial.cor <-
      function (x,
                y,
                use = c("all.obs", "complete.obs"),
                level = 1)
      {
        if (!is.numeric(x))
          stop("'x' must be a numeric variable.\n")
        y <- as.factor(y)
        if (length(levs <- levels(y)) > 2)
          stop("'y' must be a dichotomous variable.\n")
        if (length(x) != length(y))
          stop("'x' and 'y' do not have the same length")
        use <- match.arg(use)
        if (use == "complete.obs") {
          cc.ind <- complete.cases(x, y)
          x <- x[cc.ind]
          y <- y[cc.ind]
        }
        ind <- y == levs[level]
        diff.mu <- mean(x[ind]) - mean(x[!ind])
        prob <- mean(ind)
        diff.mu * sqrt(prob * (1 - prob)) / sd(x)
      }
    
    ptbiserial <- biserial.cor(x = pbx, y = pby, level = 2)
    return(ptbiserial)
  }
  
  
  ##########################################
  #                                        #
  #       Duda, pseudot2 and beale         #
  #                                        #
  ##########################################
  
  
  Indices.WKWL <- function (x, cl1 = cl1, cl2 = cl2)
  {
    dim2 <- dim(x)[2]
    wss <- function(x)
    {
      x <- as.matrix(x)
      n <- length(x)
      centers <- matrix(nrow = 1, ncol = ncol(x))
      
      if (ncol(x) == 1)
      {
        centers[1, ] <- mean(x)
      }
      if (is.null(dim(x)))
      {
        bb <- matrix(x,
                     byrow = FALSE,
                     nrow = 1,
                     ncol = ncol(x))
        centers[1, ] <- apply(bb, 2, mean)
      }
      else
      {
        centers[1, ] <- apply(x, 2, mean)
      }
      
      x.2 <- sweep(x, 2, centers[1, ], "-")
      withins <- sum(x.2 ^ 2)
      wss <- sum(withins)
      return(wss)
    }
    
    ncg1 <- 1
    ncg1max <- max(cl1)
    while ((sum(cl1 == ncg1) == sum(cl2 == ncg1)) &&
           ncg1 <= ncg1max)
    {
      ncg1 <- ncg1 + 1
    }
    g1 <- ncg1
    
    
    
    ncg2 <- max(cl2)
    nc2g2 <- ncg2 - 1
    while ((sum(cl1 == nc2g2) == sum(cl2 == ncg2)) && nc2g2 >= 1)
    {
      ncg2 <- ncg2 - 1
      nc2g2 <- nc2g2 - 1
    }
    g2 <- ncg2
    
    NK <- sum(cl2 == g1)
    WK.x <- x[cl2 == g1, ]
    WK <- wss(x = WK.x)
    
    NL <- sum(cl2 == g2)
    WL.x <- x[cl2 == g2, ]
    WL <- wss(x = WL.x)
    
    NM <- sum(cl1 == g1)
    WM.x <- x[cl1 == g1, ]
    WM <- wss(x = WM.x)
    
    duda <- (WK + WL) / WM
    
    BKL <- WM - WK - WL
    pseudot2 <- BKL / ((WK + WL) / (NK + NL - 2))
    
    beale <-
      (BKL / (WK + WL)) / (((NM - 1) / (NM - 2)) * (2 ^ (2 / dim2) -
                                                      1))
    
    results <-
      list(
        duda = duda,
        pseudot2 = pseudot2,
        NM = NM,
        NK = NK,
        NL = NL,
        beale = beale
      )
    return(results)
  }
  
  
  ####################
  #                  #
  #       ccc        #
  #                  #
  ####################
  
  
  
  Indices.WBT <- function(x, cl, P, s, vv)
  {
    n <- dim(x)[1]
    pp <- dim(x)[2]
    qq <- max(cl)
    z <- matrix(0, ncol = qq, nrow = n)
    clX <- as.matrix(cl)
    
    for (i in 1:n)
      for (j in 1:qq)
      {
        z[i, j] == 0
        if (clX[i, 1] == j)
        {
          z[i, j] = 1
        }
      }
    
    xbar <- solve(t(z) %*% z) %*% t(z) %*% x
    B <- t(xbar) %*% t(z) %*% z %*% xbar
    W <- P - B
    
    
    R2 <- 1 - sum(diag(W)) / sum(diag(P))
    v1 <- 1
    u <- rep(0, pp)
    c <- (vv / (qq)) ^ (1 / pp)
    u <- s / c
    k1 <- sum((u >= 1) == TRUE)
    p1 <- min(k1, qq - 1)
    if (all(p1 > 0, p1 < pp))
    {
      for (i in 1:p1)
        v1 <- v1 * s[i]
      c <- (v1 / (qq)) ^ (1 / p1)
      u <- s / c
      b1 <- sum(1 / (n + u[1:p1]))
      b2 <- sum(u[p1 + 1:pp] ^ 2 / (n + u[p1 + 1:pp]), na.rm = TRUE)
      E_R2 <-
        1 - ((b1 + b2) / sum(u ^ 2)) * ((n - qq) ^ 2 / n) * (1 + 4 /
                                                               n)
      ccc <-
        log((1 - E_R2) / (1 - R2)) * (sqrt(n * p1 / 2) / ((0.001 +
                                                             E_R2) ^ 1.2))
    } else
    {
      b1 <- sum(1 / (n + u))
      E_R2 <-
        1 - (b1 / sum(u ^ 2)) * ((n - qq) ^ 2 / n) * (1 + 4 / n)
      ccc <-
        log((1 - E_R2) / (1 - R2)) * (sqrt(n * pp / 2) / ((0.001 +
                                                             E_R2) ^ 1.2))
    }
    results <- ccc
    return(results)
  }
  
  
  
  ########################################################################
  #                                                                      #
  #                               Kl                                     #
  #                                                                      #
  ########################################################################
  
  
  
  
  Indices.Traces <- function(data, d, clall)
  {
    x <- data
    cl0 <- clall[, 1]
    cl1 <- clall[, 2]
    cl2 <- clall[, 3]
    clall <- clall
    nb.cl0 <- table(cl0)
    nb.cl1 <- table(cl1)
    nb.cl2 <- table(cl2)
    nb1.cl0 <- sum(nb.cl0 == 1)
    nb1.cl1 <- sum(nb.cl1 == 1)
    nb1.cl2 <- sum(nb.cl2 == 1)
    
    
    gss <- function(x, cl, d)
    {
      results <- list(wgss = NaN,
                      bgss = NaN,
                      centers = NaN)
      try({
      n <- length(cl)
      k <- max(cl)
      centers <- matrix(nrow = k, ncol = ncol(x))
      for (i in 1:k)
      {
        if (ncol(x) == 1)
        {
          centers[i, ] <- mean(x[cl == i, ])
        }
        if (is.null(dim(x[cl == i, ])))
        {
          bb <- matrix(x[cl == i, ],
                       byrow = FALSE,
                       nrow = 1,
                       ncol = ncol(x))
          centers[i, ] <- apply(bb, 2, mean)
        }
        else
        {
          centers[i, ] <- apply(x[cl == i, ,drop=FALSE], 2, mean)
        }
        
      }
      allmean <- apply(x, 2, mean)
      dmean <- sweep(x, 2, allmean, "-")
      allmeandist <- sum(dmean ^ 2)
      withins <- rep(0, k)
      x.2 <- (x - centers[cl, ]) ^ 2
      for (i in 1:k) {
        withins[i] <- sum(x.2[cl == i, ])
      }
      wgss <- sum(withins)
      bgss <- allmeandist - wgss
      
      results <- list(wgss = wgss,
                      bgss = bgss,
                      centers = centers)
      })
      return(results)
    }
    
    vargss <- function(x, clsize, varwithins)
    {
      nvar <- dim(x)[2]
      n <- sum(clsize)
      k <- length(clsize)
      varallmean <- rep(0, nvar)
      varallmeandist <- rep(0, nvar)
      varwgss <- rep(0, nvar)
      for (l in 1:nvar)
        varallmean[l] <- mean(x[, l])
      vardmean <- sweep(x, 2, varallmean, "-")
      for (l in 1:nvar) {
        varallmeandist[l] <- sum((vardmean[, l]) ^ 2)
        varwgss[l] <- sum(varwithins[, l])
      }
      varbgss <- varallmeandist - varwgss
      vartss <- varbgss + varwgss
      zvargss <- list(vartss = vartss, varbgss = varbgss)
      return(zvargss)
    }
    varwithinss <- function(x, centers, cluster) {
      nrow <- dim(centers)[1]
      nvar <- dim(x)[2]
      varwithins <- matrix(0, nrow, nvar)
      x <- (x - centers[cluster, ]) ^ 2
      for (l in 1:nvar) {
        for (k in 1:nrow) {
          varwithins[k, l] <- sum(x[cluster == k, l])
        }
      }
      return(varwithins)
    }
    
    
    
    indice.kl <-
      function (x,
                clall,
                d = NULL,
                centrotypes = "centroids") {
        if (nb1.cl1 > 0) {
          KL <- NA
        }
        if (sum(c("centroids", "medoids") == centrotypes) == 0)
          stop("Wrong centrotypes argument")
        if ("medoids" == centrotypes && is.null(d))
          stop("For argument centrotypes = 'medoids' d cannot be null")
        if (!is.null(d)) {
          if (!is.matrix(d)) {
            d <- as.matrix(d)
          }
          row.names(d) <- row.names(x)
        }
        #if (is.null(dim(x))) {
        #	    dim(x) <- c(length(x), 1)
        #}
        m <- ncol(x)
        g <- k <- max(clall[, 2])
        KL <- abs((g - 1) ^ (2 / m) * gss(x, clall[, 1], d)$wgss -
                    g ^ (2 / m) * gss(x, clall[, 2], d)$wgss) / abs((g) ^
                                                                      (2 / m) *
                                                                      gss(x, clall[, 2], d)$wgss - (g + 1) ^
                                                                      (2 / m) *
                                                                      gss(x, clall[, 3], d)$wgss)
        return(KL)
      }
    
    return(indice.kl(x, clall, d))
  }
  
  
  
  
  
  ########################################################################
  #                                                                      #
  #                             Silhouette                               #
  #                                                                      #
  ########################################################################
  
  
  
  Indice.S <- function (d, cl)
  {
    d <- as.matrix(d)
    Si <- 0
    for (k in 1:max(cl)) {
      if ((sum(cl == k)) <= 1)
        Sil <- 1
      else {
        Sil <- 0
        for (i in 1:length(cl)) {
          if (cl[i] == k) {
            ai <- sum(d[i, cl == k]) / (sum(cl == k) - 1)
            dips <- NULL
            for (j in 1:max(cl))
              if (cl[i] != j)
                if (sum(cl == j) != 1)
                  dips <-
              cbind(dips, c((sum(d[i, cl == j])) / (sum(cl ==
                                                          j))))
            else
              dips <- cbind(dips, c((sum(d[i, cl ==
                                             j]))))
            bi <- min(dips)
            Sil <- Sil + (bi - ai) / max(c(ai, bi))
          }
        }
      }
      Si <- Si + Sil
    }
    Si / length(cl)
  }
  
  
  
  
  
  
  
  
  ########################################################################
  #                                                                      #
  #                              SD, sdbw, dunn                          #
  #                                                                      #
  ########################################################################
  
  
  
  
  Index.sdindex <- function(x, clmax, cl)
  {
    x <- as.matrix(x)
    Alpha <- Dis(clmax, x)
    Scatt <- Average.scattering(cl, x)$scatt
    Dis0 <- Dis(cl, x)
    SD.indice <- Alpha * Scatt + Dis0
    return(SD.indice)
  }
  
  Index.SDbw <- function(x, cl)
  {
    x <- as.matrix(x)
    Scatt <- Average.scattering(cl, x)$scatt
    Dens.bw <- density.bw(cl, x)
    SDbw <- Scatt + Dens.bw
    return(SDbw)
  }
  
  
  #####################################################################
  #                                                                   #
  #                            Dunn index                             #
  #                                                                   #
  #####################################################################
  
  
  
  Index.dunn <-
    function(md,
             clusters,
             Data = NULL,
             method = "euclidean")
    {
      distance <- as.matrix(md)
      nc <- max(clusters)
      interClust <- matrix(NA, nc, nc)
      intraClust <- rep(NA, nc)
      
      for (i in 1:nc)
      {
        c1 <- which(clusters == i)
        for (j in i:nc) {
          if (j == i)
            intraClust[i] <- max(distance[c1, c1])
          if (j > i) {
            c2 <- which(clusters == j)
            interClust[i, j] <- min(distance[c1, c2])
          }
        }
      }
      dunn <- min(interClust, na.rm = TRUE) / max(intraClust)
      return(dunn)
    }
  
  # Funktionen zur Bestimmung der optimalen Klassenanzahl
  # maximum difference to left side
  max.left <- function(indizes) {
    
    anzahl <- length(indizes)
    k.min <- as.numeric(names(indizes)[1])
    k.max <- as.numeric(names(indizes)[anzahl])
    
    
    diffs <- rep(0,anzahl-1)
    
    for (i in 1:anzahl-1) {
      diffs[i] <- indizes[i+1] - indizes[i]
    }
    
    res <- which(diffs==max(diffs))[1]+k.min
    
    as.numeric(res)
    
  }
  
  # maximum difference to right side
  max.right <- function(indizes) {
    
    
    
    anzahl <- length(indizes)
    k.min <- as.numeric(names(indizes)[1])
    k.max <- as.numeric(names(indizes)[anzahl])
    
    diffs <- rep(0,anzahl-1)
    
    for (i in 1:anzahl-1) {
      diffs[i] <- indizes[i] - indizes[i+1]
    }
    
    res <- which(diffs==max(diffs))[1]+k.min-1
    
    as.numeric(res)
    
  }
  
  # maximum of second differences
  max.second <- function(indizes) {
    
    
    anzahl <- length(indizes)
    k.min <- as.numeric(names(indizes)[1])
    k.max <- as.numeric(names(indizes)[anzahl])
    
    diffs <- rep(0,anzahl-2)
    
    for (i in 1:(anzahl-2)) {
      diffs[i] <- (indizes[i+2] - indizes[i+1]) - (indizes[i+1] - indizes[i])
    }
    
    res <- which(diffs==max(diffs))[1]+k.min
    
    as.numeric(res)
    
  }
  
  # minimum of second differences
  min.second <- function(indizes) {
    
    
    anzahl <- length(indizes)
    k.min <- as.numeric(names(indizes)[1])
    k.max <- as.numeric(names(indizes)[anzahl])
    
    diffs <- rep(0,anzahl-2)
    
    for (i in 1:(anzahl-2)) {
      diffs[i] <- (indizes[i+2] - indizes[i+1]) - (indizes[i+1] - indizes[i])
    }
    
    res <- which(diffs==min(diffs))[1]+k.min
    
    as.numeric(res)
    
  }
  
  # maximale index
  max.index <- function(indizes) {
    
    k.min <- as.numeric(names(indizes)[1])
    
    res <- which(indizes==max(indizes))[1]
    
    res <- res + k.min - 1
    
    as.numeric(res)
    
  }
  
  # minimale index
  min.index <- function(indizes) {
    
    k.min <- as.numeric(names(indizes)[1])
    
    res <- which(indizes==min(indizes))[1]
    
    res <- res + k.min - 1
    
    as.numeric(res)
    
  }
  
  ####
  
  #Berechnung der Kennzahlen ----
  res <- matrix(data = 0,
                nrow = range,
                ncol = length(indexnames))
  colnames(res) <- indexnames
  rownames(res) <- c(min.nc:max.nc)
  criticalValues <- matrix(data = 0,
                           nrow = range,
                           ncol = 3)
  colnames(criticalValues) <- c(indexnames[18:20])
  rownames(criticalValues) <- c(min.nc:max.nc)
  
  if (any(indexn == 2) || indexn == all) {
    distdata <- count(data)
  }
  
  for (i in 0:(range - 1)) {
    #temp <- rep(min.nc+i,length(indexnames))
    #temp <- rep(min.nc+i,4)
    
    temp1 <- rep(0, 14)
    temp2 <- list(Kennzahlen = rep(0, 12),
                  criticalValues = rep(0, 3))
    if (any(indexn <= 14) || indexn == all) {
      clstemp <- clusters[, i + 1]
      center <- centers(clstemp)
      withins <- withinss(center, clstemp)
      size <- hist(clstemp, c(0, unique(clstemp)), plot = F)$counts
      clres <- list(
        centers = center,
        withinss = withins,
        size = size,
        cluster = clstemp
      )
      
      zgss <- gss(data, clres$size, clres$withins)
      zttw <- ttww(data, clres$size, clres$cluster)
    }
    
    #Kennzahlen aus cclust
    if (any(indexn == 1) || indexn == all) {
      res[i + 1, 1] <- calinski(zgss, clres$size)
    }
    if (any(indexn == 2) || indexn == all) {
      minmaxd <- maxmindist(clres$size, distdata)
      res[i + 1, 2] <- cindex(clres$withins, minmaxd,
                              clres$size)
    }
    if (any(indexn == 3) || indexn == all) {
      res[i + 1, 3] <- db(clres$withins, clres$centers, clres$cluster)
    }
    if (any(indexn == 4) || indexn == all) {
      res[i + 1, 4] <- hartigan(zgss)
    }
    if (any(indexn == 5) || indexn == all) {
      varwithins <- varwithinss(data, clres$centers, clres$cluster)
      zvargss <- vargss(data, clres$size, varwithins)
      res[i + 1, 5] <- ratkowsky(zvargss, clres$size)
    }
    if (any(indexn == 6) || indexn == all) {
      res[i + 1, 6] <- scott(zttw, clres$size)
    }
    if (any(indexn == 7) || indexn == all) {
      res[i + 1, 7] <- marriot(zttw, clres$size)
    }
    if (any(indexn == 8) || indexn == all) {
      res[i + 1, 8] <- ball(clres$withins, clres$size)
    }
    if (any(indexn == 9) || indexn == all) {
      res[i + 1, 9] <- tracecovw(zttw)
    }
    if (any(indexn == 10) || indexn == all) {
      res[i + 1, 10] <- tracew(zttw)
    }
    if (any(indexn == 11) || indexn == all) {
      res[i + 1, 11] <- friedman(zttw)
    }
    if (any(indexn == 12) || indexn == all) {
      res[i + 1, 12] <- rubin(zttw)
    }
    if (any(indexn == 13) || indexn == all) {
      res[i + 1, 13] <- ssi(clres$centers, clres$size)$ssiw
    }
    if (any(indexn == 14) || indexn == all) {
      res[i + 1, 14] <- xu(data, clres$size, zgss)
    }
    #Kennzahlen aus NbClust
    if (any(indexn >= 15) || indexn == all) {
     
      jeu <- data
      nn <- numberObsAfter <- dim(jeu)[1]
      pp <- dim(jeu)[2]
      TT <- t(jeu) %*% jeu
      sizeEigenTT <- length(eigen(TT)$value)
      eigenValues <- eigen(TT / (nn - 1))$value
      
      # Only for indices using vv : CCC
      
      if (any(indexn == 16) || indexn == all)
      { try({
        for (i400 in 1:sizeEigenTT)
        {
          if (eigenValues[i400] < 0) {
            #cat(paste("There are only", numberObsAfter,"nonmissing observations out of a possible", numberObsBefore ,"observations."))
            stop(
              "The TSS matrix is indefinite. There must be too many missing values. The index cannot be calculated."
            )
          }
        }
        s1 <- sqrt(eigenValues)
        ss <- rep(1, sizeEigenTT)
        for (i500 in 1:sizeEigenTT)
        {
          if (s1[i500] != 0)
            ss[i500] = s1[i500]
        }
        vv <- prod(ss)
      })
      }
      md <- dist(jeu, method = "euclidean")
      
      cl0 <- clusters2[, i + 1]
      cl1 <- clusters[, i + 1]
      cl2 <- clusters2[, i + 3]
      clmax <- clusters[, range]
      
      clall <- cbind(cl1, cl2)
      clall1 <- cbind(cl0, cl1, cl2)
    }
    
    
    if (any(indexn == 15) || indexn == all)
    {
      res[i + 1, 15] <- Indices.Traces(jeu, md, clall1)
    }
    if (any(indexn == 16) || indexn == all)
    {
      res[i + 1, 16]=NaN
      try({
      res[i + 1, 16] <- Indices.WBT(
        x = jeu,
        cl = cl1,
        P = TT,
        s = ss,
        vv = vv
      )
      })
    }
    if (any(indexn == 17) || indexn == all)
    {
      res[i + 1, 17] <- Indice.S(d = md, cl = cl1)
    }
    if (any(indexn == 18) ||
        any(indexn == 19) || any(indexn == 20) || indexn == all)      {
      temp <-     Indices.WKWL(x = jeu, cl1 = cl1, cl2 = cl2)
      
      
      res[i + 1, 18] <- temp$duda
      res[i + 1, 19] <- temp$pseudot2
      res[i + 1, 20] <- temp$beale
      NM <- temp$NM
      NK <- temp$NK
      NL <- temp$NL
      zz <- 3.20 # Best standard score in Milligan and Cooper 1985
      zzz <- zz * sqrt(2 * (1 - 8 / ((pi ^ 2) * pp)) / (NM * pp))
      
      
      
      if (any(indexn == 18) || indexn == all)
      {
        criticalValues[i + 1, 1] <- critValue <- 1 - (2 / (pi * pp)) - zzz
      }
      
      if (any(indexn == 19) || indexn == all)
      {
        critValue <- 1 - (2 / (pi * pp)) - zzz
        criticalValues[i + 1, 2] <-
          ((1 - critValue) / critValue) * (NK + NL - 2)
        
      }
      
      
      if (any(indexn == 20) || indexn == all)
      {
        df2 <- (NM - 2) * pp
        criticalValues[i + 1, 3] <- 1 - pf(temp$beale, pp, df2)
      }
      
    }
    if (any(indexn == 21) || indexn == all)
    {
      res[i + 1, 21] <- Indice.ptbiserial(x = jeu, md = md, cl1 = cl1)
    }
    if (any(indexn == 22) || any(indexn == 23) || indexn == all)
    {
      
      temp <- Index.15and28(cl1 = cl1,
                            cl2 = cl2,
                            md = md)
      res[i + 1, 22] <- temp$frey
      res[i + 1, 23] <- temp$mcclain
      
    }
    if (any(indexn == 24) || indexn == all)
    {
      res[i + 1, 24] <- Index.dunn(md, cl1, Data = jeu, method = NULL)
    }
    if (any(indexn == 25) || indexn == all)
    {
      res[i + 1, 25] <- Index.sdindex(jeu, clmax, cl1)
    }
    if (any(indexn == 26) || indexn == all)
    {
      res[i + 1, 26] <- Index.SDbw(jeu, cl1)
    }
    
    if (!Silent) {
      print(paste0("Kennzahlen fuer Klassenanzahl ",i+min.nc," berechnet, hoechste Klassenanzahl: ",max.nc))
    }
    
  }
  
  if (!Silent) {
    print("Kennzahlen berechnet, ermittle optimale Klassenanzahlen")
  }
  
  
  
  #Bestimmen der optimalen Klassenanzahl
  klassenanzahl <-
    matrix(data = 0,
           nrow = 1,
           ncol = length(indexnames))
  colnames(klassenanzahl) <- indexnames
  rownames(klassenanzahl) <- c("empfohlene Klassenanzahl")
  
  if (any(indexn == 1) || indexn == all) {
    #calinski
    klassenanzahl[1] <- min.second(res[, 1])
  }
  if (any(indexn == 2) || indexn == all) {
    #cindex
    klassenanzahl[2] <- max.second(res[, 2])
  }
  if (any(indexn == 3) || indexn == all) {
    #db
    klassenanzahl[3] <- min.index(res[, 3])
  }
  if (any(indexn == 4) || indexn == all) {
    #hartigan
    klassenanzahl[4] <- min.second(res[, 4])
  }
  if (any(indexn == 5) || indexn == all) {
    #ratkowsky
    klassenanzahl[5] <- max.right(res[, 5])
  }
  if (any(indexn == 6) || indexn == all) {
    #scott
    klassenanzahl[6] <- max.left(res[, 6])
  }
  if (any(indexn == 7) || indexn == all) {
    #marriot
    klassenanzahl[7] <- max.second(res[, 7])
  }
  if (any(indexn == 8) || indexn == all) {
    #ball
    klassenanzahl[8] <- max.second(res[, 8])
  }
  if (any(indexn == 9) || indexn == all) {
    #trocw
    klassenanzahl[9] <- min.second(res[, 9])
  }
  if (any(indexn == 10) || indexn == all) {
    #tracew
    klassenanzahl[10] <- max.second(res[, 10])
  }
  if (any(indexn == 11) || indexn == all) {
    #friedman
    klassenanzahl[11] <- max.left(res[, 11])
  }
  if (any(indexn == 12) || indexn == all) {
    #rubin
    klassenanzahl[12] <- min.second(res[, 12])
  }
  if (any(indexn == 13) || indexn == all) {
    #ssi
    klassenanzahl[13] <- max.index(res[, 13])
  }
  if (any(indexn == 14) || indexn == all) {
    #xuindex
    klassenanzahl[14] <- max.second(res[, 14])
  }
  if (any(indexn == 15) || indexn == all) {
    #kl
    klassenanzahl[15] <- max.index(res[, 15])
  }
  if (any(indexn == 16) || indexn == all) {
    #ccc
    klassenanzahl[16] <- max.index(res[, 16])
  }
  if (any(indexn == 17) || indexn == all) {
    #silhouette
    klassenanzahl[17] <- max.index(res[, 17])
  }
  if (any(indexn == 18) || indexn == all) {
    #duda
    klassenanzahl[18] <- NA
    flag <- FALSE
    c <- 0
    while (!flag) {
      if (res[c + 1, 18] >= criticalValues[c + 1, 1]) {
        flag <- TRUE
        klassenanzahl[18] <- c + min.nc
      }
      else if (c + min.nc == max.nc) {
        flag <- TRUE
      }
      c <- c + 1
    }
  }
  if (any(indexn == 19) || indexn == all) {
    #pseudot2
    klassenanzahl[19] <- NA
    flag <- FALSE
    c <- 0
    while (!flag) {
      if (res[c + 1, 19] <= criticalValues[c + 1, 2]) {
        flag <- TRUE
        klassenanzahl[19] <- c + min.nc
      }
      else if (c + min.nc == max.nc) {
        flag <- TRUE
      }
      c <- c + 1
    }
  }
  if (any(indexn == 20) || indexn == all) {
    #beale
    klassenanzahl[20] <- NA
    flag <- FALSE
    c <- 0
    while (!flag) {
      if (criticalValues[c + 1, 3] >= alphabeale) {
        flag <- TRUE
        klassenanzahl[20] <- c + min.nc
      }
      else if (c + min.nc == max.nc) {
        flag <- TRUE
      }
      c <- c + 1
    }
  }
  if (any(indexn == 21) || indexn == all) {
    #ptbiserial
    klassenanzahl[21] <- max.index(res[, 21])
  }
  if (any(indexn == 22) || indexn == all) {
    #frey
    klassenanzahl[22] <- 1
    flag <- FALSE
    c <- 0
    while (!flag) {
      if (res[c + 1, 22] < 1) {
        flag <- TRUE
        klassenanzahl[22] <- c + min.nc - 1
      }
      else if (c + min.nc == max.nc) {
        flag <- TRUE
      }
      c <- c + 1
    }
  }
  if (any(indexn == 23) || indexn == all) {
    #mcclain
    klassenanzahl[23] <- min.index(res[, 23])
  }
  if (any(indexn == 24) || indexn == all) {
    #dunn
    klassenanzahl[24] <- max.index(res[, 24])
  }
  if (any(indexn == 25) || indexn == all) {
    #sdindex
    klassenanzahl[25] <- min.index(res[, 25])
  }
  if (any(indexn == 26) || indexn == all) {
    #sdbw
    klassenanzahl[26] <- min.index(res[, 26])
  }
  
  
  
  
  if (is.null(crits)) {
    criticalValues <- NA
  }
  else if (length(crits) == 1) {
    criticalValues <- as.matrix(criticalValues[, crits])
    colnames(criticalValues) <- c(indexnames[18:20])[crits]
  }
  else {
    criticalValues <- criticalValues[, crits]
  }
  
  if (all(indexn != all)) {
    if (length(indexn) == 1) {
      res <- as.matrix(res[, indexn])
      colnames(res) <- indexnames[indexn]
      
      
    }
    else {
      res <- res[, indexn]
    }
    klassenanzahl <- as.matrix(klassenanzahl[, indexn])
    rownames(klassenanzahl) <- indexnames[indexn]
    colnames(klassenanzahl) <- c("empfohlene Klassenanzahl")
  }
  else {
    klassenanzahl <- t(klassenanzahl)
  }
  
  if (!Silent) {
    print("optimale Klassenanzahlen per Verfahren ermittelt - ENDE")
  }
  if(isTRUE(PlotIt)){
    cat=paste('Cluster No.',klassenanzahl)
    requireNamespace('DataVisualizations')
    DataVisualizations::Fanplot(cat,main = 'Indicators for Cluster No.',MaxNumberOfSlices = SelectByABC)
  }
  resliste <- list(
    Indicators = res,
    ClusterNo = klassenanzahl,
    ClsMatrix = clusters,
    HierarchicalIndicators = criticalValues
  )
  
  return(resliste)
}