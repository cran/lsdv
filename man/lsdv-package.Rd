\name{lsdv-package}
\alias{lsdv-package}
\alias{lsdv}
\docType{package}
\title{Fixed effect panel data regression}
\description{Fit a least square dummy variable regression model.} 
\details{
\tabular{ll}{
Package: \tab lsdv\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-06-28\cr
License: \tab GPL(>=3.0.1)\cr
LazyLoad: \tab yes\cr
}
}
\author{
Zaghdoudi Taha

Zaghdoudi Taha <zedtaha@gmail.com>
}
\references{
Amemiyia, T. (1971) The estimation of the variances in a
  variance--components model, \emph{International Economic Review}, \bold{12},
  pp.1--13.
  
   Baltagi, B.H. (1981) Simultaneous equations with error components,
  \emph{Journal of econometrics}, \bold{17}, pp.21--49.
  
  Baltagi, B.H. (2001) \emph{Econometric Analysis of Panel Data}. John
  Wiley and sons. ltd.
  
}

\keyword{ package }
\examples{

#Create some data 
pib<-as.matrix(c(12,3,4,0.4,0.7,5,0.7,0.3,0.6,89,7,8,45,7,4,5,0.5,5),nrows=18,ncols=1)
tir<-as.matrix(c(12,0.3,4,0.4,7,12,3.0,6.0,45,7.0,0.8,44,65,23,4,6,76,9),nrows=18,ncols=1)
inf<-as.matrix(c(1.2,3.6,44,1.4,0.78,54,0.34,0.66,12,0.7,8.0,12,65,43,5,76,65,8),nrows=18,ncols=1)
npl<-as.matrix(c(0.2,3.8,14,2.4,1.7,43,0.2,0.5,23,7.8,88,36,65,3,44,65,7,34),nrows=18,ncols=1)
# create a data frame 
mdata<-data.frame(p=pib,t=tir,int=inf,np=npl)
# fit the fixed function   
fp<-fixed(t~p+int+np,mdata,n=6,T=3)
fp
summary(fp)
}
