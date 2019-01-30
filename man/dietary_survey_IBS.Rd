\name{dietary_survey_IBS}
\alias{dietary_survey_IBS}

\docType{data}
\title{
dietary survey IBS [Hayes et al., 2013]
}
\description{
Two classes density based data set taken from \pkg{clusterR}.

}
\usage{data("dietary_survey_IBS")}

\details{
Size 400, Dimensions 42, stored in \code{dietary_survey_IBS$Data}

Classes 2, stored in \code{dietary_survey_IBS$Cls}: healthy-group (class == 1) vs. the IBS-patients (class == 2)




"The data are based on the article "A dietary survey	of patients	with irritable bowel syndrome". The mean and standard deviation of the table 1 (Foods perceived as causing or worsening irritable bowel syndrome symptoms in the IBS group and digestive symptoms in the healthy comparative group) were used to generate the synthetic data" [Hayes et al., 2013].
}

\references{
[Hayes et al., 2013]		P. Hayes, C. Corish, E. O'Mahony, E. M. M. Quigley: A dietary survey of patients with irritable bowel syndrome, Journal of Human Nutrition and Dietetics,2013.
}
\examples{
data(dietary_survey_IBS)
str(dietary_survey_IBS)
\donttest{
Data=dietary_survey_IBS$Data
Cls=dietary_survey_IBS$Cls
# The Pswarm is able to seperate density-based clusters
res=Pswarm(RobustNormalization(Data,T),PlotIt = T,Cls = Cls)
# We clearly see that the U-Matrix visualization is unable to catch this structures
# because it is only usuable for distance structures

resU=GeneratePswarmVisualization(Data,res$ProjectedPoints,res$LC,PlotIt = T)
}
}
\keyword{datasets,dietary_survey_IBS,FCPS}