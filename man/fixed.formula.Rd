\name{fixed.formula}
\alias{fixed.formula}

\title{Fixed effect formula}
\description{here we use a formula to fit the fixed effect.} 
\usage{
\method{fixed}{formula}(formula, data = list(), n, T, ...)
}

\arguments{
  
\item{formula}{ a symbolic description of the model to be fit. }

  \item{data}{
an optional data frame containing the variables in the model.
}
   \item{n}{
number of cross-sections
}
  \item{T}{
times periods
}
  \item{\dots}{
not used
}
}
\value{
\item{coefficients}{ a named vector of coefficients }
\item{vcov}{ covariance matrix of coefficients }
\item{fitted.values}{ fitted values }
\item{residuals}{ residuals }
\item{Fstat}{simple fisher test}
\item{Feffect}{ fisher test of effects}
\item{Rsquared}{ the Rsquared}
}

\author{
Zaghdoudi Taha
}

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
summary(fp)
}

