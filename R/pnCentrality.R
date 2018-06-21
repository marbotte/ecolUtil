pnCentrality <-
function(posit,negat)
{
stopifnot(nrow(posit)==ncol(posit),nrow(posit)==nrow(negat),nrow(negat)==ncol(negat))
A<-posit-(2*negat)
n<-nrow(posit)
return(solve(diag(n)-((1/(2*n-2))*A))%*%rep(1,n))
}
