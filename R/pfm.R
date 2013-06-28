pfm <-
function(x,y,n,T){
n<-n
T<-T
b<-1/T
In<-diag(n)
ex<-matrix(b,nrow=T,ncol=T)
E<-kronecker(In,ex)
It<-diag(T*n)
Q<-It-E
qx<-t(x)%*%Q%*%x
qy<-t(x)%*%Q%*%y
qq<-qr(qx)
beta<-solve.qr(qq,qy)
residus<-Q%*%y-Q%*%x%*%beta
df<-nrow(x)-ncol(x)
qxq<-ginv(qx)
scr<-sum((residus)^2)
sct<-t(y)%*%Q%*%y
sigma2 <- sum((residus)^2)/df
vcov <- sigma2 * qxq
sigmae<-sqrt(sigma2)
raq<-1-scr/sct
p<-ncol(x)
fstat<-(sct-scr)*(n-p-1)/(scr*p)
df2<-n-p-1
pfstat<- pf(fstat, p, df2, lower.tail = TRUE, log.p = FALSE)
#MCO normale
xq<-qr(x)
alfa<-solve.qr(xq,y)
error<-y-x%*%alfa
scre<-sum((error)^2)
effet<-(scre-scr)*(n*T-n-p)/(scr*(n-1))
dff<-n-1
dff1<-n*T-n-p
peffet<-pf(effet,dff,dff1, lower.tail = TRUE, log.p = FALSE)
list(coefficients=beta,vcov=vcov,sigma=sigmae,Rsquared=raq,Fstat=fstat,Pvalue=pfstat, Feffect=effet, Pveffet= peffet,df=df)
}
