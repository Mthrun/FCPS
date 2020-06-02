SpectralClustering <- function(Data, ClusterNo,PlotIt=FALSE,...){
  #Cls=SpectralClustering(Data,ClusterNo)
  #Cls=SpectralClustering(Data,ClusterNo,...)
  # Clusters the Data into "ClusterNo" different clusters using the Spectal Clustering Method
  # 
  # INPUT
  # Data(1:n,1:m)           Data to be clustered. n Datapoints with m Attributes
  # ClusterNo    Number of different Clusters to build
  #
  # OPTIONAL
  # kernel		              Kernelmethod, possible options:
  #                         default:        unknown!, maybe kernel="rbfdot"
  #                         rbfdot          Radial Basis kernel function "Gaussian"
  #                         polydot         Polynomial kernel function
  #                         vanilladot      Linear kernel function
  #                         tanhdot         Hyperbolic tangent kernel function
  #                         laplacedot      Laplacian kernel function
  #                         besseldot       Bessel kernel function
  #                         anovadot        ANOVA RBF kernel function
  #                         splinedot       Spline kernel
  #                         stringdot       String kernel
  # kpar			Kernelparameter: a character string or the list of hyper-parameters 
  #                             (kernel parameters). The default character string
  #           "automatic"   uses a heuristic to determine a suitable value for the 
  #                         width parameter of the RBF kernel.
  #           "local"       (local scaling) uses a more advanced heuristic and 
  #                         sets a width parameter for every point in the data set.
  #           A list can also be used containing the parameters to be used with the 
  #           kernel function.
  # nystrom.red     use nystrom method to calculate eigenvectors. When TRUE a 
  #                 sample of the dataset is used to calculate the eigenvalues, 
  #                 thus only a n x m matrix where n the sample size is stored in 
  #                 memory
  #                 if TRUE, use furhter arguments: nystrom.sample,
  #                 further possible arguments: 
  #                 mod.sample proportion of data to use when estimating sigma #                 (default: 0.75)
  #                 na.action	the action to perform on NA
  # OUTPUT
  # Cls[1:n]    Clustering of data
  # Object      Object of kernlab::specc algorithm
  #
  # Author: MT 04/2018 (redone from new)
  #
  # NOTA
  # see http://artax.karlin.mff.cuni.cz/r-help/library/kernlab/html/specc.html
  # for details about kernel functions and valid parameters for list in kpar
  # publication
  # On  Spectral  Clustering:  Analysis  and  an  algorithm
  # Andrew  Y. Ng, Michael  I.  Jordan , Yair  Weiss, 2001
  #package: kernlab
  
 # if(!require(kernlab)){
 #   install.packages('kernlab')
 #   library(kernlab)
#  }
  #SpectralClustering <- function(Data, ClusterNo=2,kernel = "rbfdot", kpar="automatic",nystrom.red=F,nystrom.sample = dim(Data)[1]/6,iterations = 200,mod.sample = 0.75, na.action = na.omit,...){
  if (!requireNamespace('kernlab')) {
    message(
      'Subordinate clustering package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate clustering package is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }

	sc=kernlab::specc(Data, centers=ClusterNo,...)
  #cls <- matrix(kernlab::specc(Data, centers=K,...))
	Cls=sc@.Data
	if(PlotIt){
	  ClusterPlotMDS(Data,Cls)
	}  
	Cls=ClusterRename(Cls,Data)
	return(list(Cls=Cls,Object=sc))
}
