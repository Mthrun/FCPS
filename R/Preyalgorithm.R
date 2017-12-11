Preyalgorithm <- function(Data, Nclusters, Metric, Cls, MatlabExe){
  #out = Preyalgorithm(Hepta$Data,7,MatlabExe = 'D:/appz/Matlab2012a/bin/matlab.exe')
  # Description
  # Projection and clustering method based on based on foraging theory
  # 
  # INPUT
  # Data(1:n,1:d)             n cases, d variables. matrix of data in file, no NaN are allowed. Dissimilarity matrix will be generated
  # Nclusters					Number of classes in the dataset
  #
  # OPTIONAL
  # metric				    string for dissimilarity: 'euclidean' (default); 'cosine', 'mahalanobis', 'locality-improved_kernel', 'polynomial_kernel'
  # Cls(1:n)          	    labels, column integer vector of Classes: only for intern plotting of progress not for computations of prey algorithm
  # Output
  # A list containing two objects: lrn and cls, containing the projected data and the new classification respectively.
  
  # prey algorithm, see: Giraldo, L. F., Lozano, F., & Quijano, N.: Foraging theory for dimensionality reduction of clustered data, Machine Learning, Vol. 82(1), pp. 71-90. 2011.
  
  #Details:
  #  foraging theory studies two basic problems: which prey an forager should consume and when to leave
  # a patch [Stephens/Krebs, 1986, p. 6]. 
  # In artificial life, the forages is viewed as an agent who compares the potential energy gain
  # to the potential opportunity of finding an item of superior type [Martens et al., 2011] cites [Stephens/Krebs, 1986]. 
  # This approach is also called prey model [Martens et al., 2011]: the average energy gain 
  # can be mathematically expressed in the terms of expected time, energy intake, encounter rate
  # and attack probability for each type of prey. In the unsupervised machine learning method of [Giraldo et al., 2011], 
  # additionally, the foraging landscape was viewed as a discrete space as, and objects representing 
  # points from the dataset as prey. Three agents were defined as foragers. 
  
  Filename='outtmptmptmptmp' # This is to make it quite improbable that the name for the temporary files is taken. Otherwise we will still break later.
  Outdirectory=getwd()
  
  towindir <- function(path){
    return(paste0(paste0(strsplit(path, split = "/")[[1]], collapse = "\\"), "\\"))
  }
  
  if(missing(Data) || missing (Nclusters))
    stop("Data and Nclusters required")
  
  if(missing(Metric))
    Metric = 'euclidean'
  
  if(!(Metric %in% c( 'euclidean', 'cosine', 'mahalanobis', 'locality-improved_kernel', 'polynomial_kernel')))
    stop("Metric MUST be one of the following:  'euclidean' (default); 'cosine', 'mahalanobis', 'locality-improved_kernel', 'polynomial_kernel'")
  
  files = c("tmptmptmptmp.lrn", "tmptmptmptmp.cls", paste0(Filename,".lrn"), paste0(Filename,".cls"), ".Preylock")
  existing = file.exists(files)
  if(any(existing))
     stop(paste0("Please delete/move/rename the following files, because these filenames are used to communicate with matlab: ",
                 files[existing], collapse = " "))
  
  # Preparation
  currdir <- getwd()
  currdirmatlab = towindir(currdir);
  WriteLRN("tmptmptmptmp.lrn", Data = Data)
  if(!missing(Cls))
  {
    WriteCLS("tmptmptmptmp.cls", Cls = Cls)
    clsparam = ", cls"
  } else {
    clsparam = ""
  }
  if(missing(MatlabExe))
  {
    MatlabExe = "C:/MATLAB701/bin/win32/MATLAB.exe"
  }
  Preypath = towindir(paste0(SubversionDirectory(),"PUB/dbt/ClusteringAlgorithms/.PreyClustering/"))
  
  # Create the parts of the string we want to send to Matlab
  matlabpath = paste0("addpath('", Preypath, "')")
  switchdir  = paste0("cd('", currdirmatlab,"')")
  read = paste0("lrn = ReadLRN('tmptmptmptmp.lrn','",currdirmatlab,"');")
  if(missing(Cls))
  { readcls = ""}
  else
  { readcls = paste0("; cls = ReadCLS('tmptmptmptmp.cls','",currdirmatlab,"');")}
  
  preystring  = paste0("[X_prey,LabelsMap,error] = preyalgorithm(lrn, ",Nclusters, ", '",  Metric, "'", clsparam, ")")
  lrnheader   = "Header=char('X','Y')"
  lrnproj     = "Proj=X_prey(:,[1 2])"
  writeout    = paste0("WriteLRN('",Filename,".lrn', Proj, Header,[],[], '", towindir(Outdirectory), "', num2str(error))")
  writeoutlrn = paste0("WriteCLS('",Filename,".cls', LabelsMap, [], '",towindir(Outdirectory), "')")
  
  
  # construct final string
  matlabscript = paste0("\"try, ", matlabpath, ", ", switchdir, ", ", read, readcls, ", ", preystring,", ", lrnheader, ", ", lrnproj,
                        ", ", writeout, ", ", writeoutlrn, ", catch, delete('.Preylock'), exit(), end, delete('.Preylock'), exit();\"")

  # Since we need to wait for Matlab to finish calculations (and system2() does not wait for that) we create a lockfile
  file.create(".Preylock")
  
  system2(MatlabExe, args = c("-nodisplay", "-nojvm", "-nosplash", "-1", "-r", matlabscript),wait = T,stdout=F)
  
  # Now wait until Matlab has finished
  while(file.exists(".Preylock"))
    Sys.sleep(0.5)
  
  # Check if matlab result is present
  if(!file.exists(paste0(Filename,".lrn")))
    stop("Something went wrong in the matlab part")

  # remove temporary files
  file.remove("tmptmptmptmp.lrn")
  if(!missing(Cls))
    file.remove("tmptmptmptmp.cls")

  # read results and remove matlab outputfiles
  output = ReadLRN(paste0(Filename,".lrn"))
  outputcls = ReadCLS(paste0(Filename,".cls"))
  file.remove(paste0(Filename,".lrn"))
  file.remove(paste0(Filename,".cls"))
  
  return(list(ProjectedPoints = output$Data, Cls = outputcls$Cls))
}