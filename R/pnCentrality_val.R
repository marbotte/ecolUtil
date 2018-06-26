pnCentrality_val<-function(posit,negat,fac_beta=max(c(posit,negat))*2)
{
	stopifnot(nrow(posit)==ncol(posit),nrow(posit)==nrow(negat),nrow(negat)==ncol(negat),any(!posit<0),any(!negat<0))
	A<-posit-(2*negat)
	n<-nrow(posit)
	return(solve(diag(n)-((1/(fac_beta*n-fac_beta))*A))%*%rep(1,n))
}
