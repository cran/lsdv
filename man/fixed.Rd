\name{fixed}
\alias{fixed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fixed effect panel data regression}
\description{Fit a least square dummy variable regression model.} 
\usage{
fixed(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numeric design matrix for the model.
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
}

\author{
Zaghdoudi Taha
}

\examples{
function(x,...)
{UseMethod("fixed") }
}
\keyword{ fixed }

