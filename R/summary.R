summary<-
function(object, ...)
{
se <- sqrt(diag(object$vcov))
tval <-coef(object)/se
TAB <- cbind(Estimate = coef(object),StdErr = se, t.value = tval, p.value = 2*pt(-abs(tval), df=object$df))
colnames(TAB) <- c("Estimate", "Std.Err", "T value", "Pr(>z)")
res <- list(call=object$call,
coefficients=TAB)
class(res) <- "summary.fixed"
res
}