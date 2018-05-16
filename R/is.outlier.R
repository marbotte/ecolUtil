is.outlier <-
function(x,fac=3)
{
quartiles<-quantile(x,c(.25,.5,.75))
return(x<(quartiles[2]-(fac*(quartiles[3]-quartiles[1])))|x>quartiles[2]+(fac*(quartiles[3]-quartiles[1])))
}
