\name{ginv}
\alias{ginv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inversion of matrix
}
\description{
inverse matrix
}
\usage{
ginv(x, tol = sqrt(.Machine$double.eps))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
matrix to inverse
}
  \item{tol}{
A relative tolerance to detect zero singular values.
}
}

\value{
A MP generalized inverse matrix for X.
}
\references{
Venables, W. N. and Ripley, B. D. (1999) Modern Applied Statistics with S-PLUS. Third Edition. Springer. p.100.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, tol = sqrt(.Machine$double.eps)) 
{
    dnx <- dimnames(x)
    if (is.null(dnx)) 
        dnx <- vector("list", 2)
    s <- svd(x)
    nz <- s$d > tol * s$d[1]
    structure(if (any(nz)) 
        s$v[, nz] \%*\% (t(s$u[, nz])/s$d[nz])
    else x, dimnames = dnx[2:1])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ginv }

