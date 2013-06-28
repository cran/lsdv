fixed.default <-
function(x,y,n,T,...)
{
n<-n
T<-T
x <- as.matrix(x)
y <- as.numeric(y)
est <- pfm(x,y,n,T)
est$fitted.values <- as.vector(x %*% est$coefficients)
est$residuals <- y - est$fitted.values
est$call <- match.call()
class(est) <- "fixed"
est
}
