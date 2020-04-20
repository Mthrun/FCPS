\name{StatPDEdensity}
\alias{StatPDEdensity}

\title{
Pareto Density Estimation
}
\description{
Density Estimation for ggplot with a clear model behind it.
}

\format{
  The format is:
Classes 'StatPDEdensity', 'Stat', 'ggproto' <ggproto object: Class StatPDEdensity, Stat>
    aesthetics: function
    compute_group: function
    compute_layer: function
    compute_panel: function
    default_aes: uneval
    extra_params: na.rm
    finish_layer: function
    non_missing_aes: 
    parameters: function
    required_aes: x y
    retransform: TRUE
    setup_data: function
    setup_params: function
    super:  <ggproto object: Class Stat> 
}
\details{
PDE was published in [Ultsch, 2005], short explanation in [Thrun, Ultsch 2018] and the PDE optimized violin plot was published in [Thrun et al., 2018].
}

\references{
[Ultsch,2005]  Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, in  Baier, D.; Werrnecke, K. D., (Eds), Innovations in classification, data science, and information systems, Proc Gfkl 2003, pp 91-100, Springer, Berlin, 2005.

[Thrun, Ultsch 2018]   Thrun, M. C., & Ultsch, A. : Effects of the payout system of income taxes to municipalities in Germany, in Papiez, M. & Smiech,, S. (eds.), Proc. 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, pp. 533-542, Cracow: Foundation of the Cracow University of Economics, Cracow, Poland, 2018. 

[Thrun et al, 2018]	Thrun, M. C., Pape, F., & Ultsch, A. : Benchmarking Cluster Analysis Methods using PDE-Optimized Violin Plots, Proc. European Conference on Data Analysis (ECDA), accepted, Paderborn, Germany, 2018. 

}

\keyword{density estimation}
\keyword{ggproto density estimation}
\keyword{PDE}
\keyword{Pareto Density Estimation}