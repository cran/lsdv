fixed.formula <-
function(formula, data=list(),n,T, ...)
{
n<-n
T<-T
mf <- model.frame(formula=formula, data=data)
x <- model.matrix(attr(mf, "terms"), data=mf)
y <- model.response(mf)
est <- fixed.default(x,y,n,T,...)
est$call <- match.call()
est$formula <- formula
est
}
