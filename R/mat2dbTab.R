mat2dbTab <-
function(mat)
{
  W<-which(mat>0,arr.ind<-T)
  dbTab<-data.frame(SU=rownames(mat)[W[,"row"]],sp=colnames(mat)[W[,"col"]],ab=mat[W])
  dbTab<-dbTab[order(dbTab$SU,dbTab$sp),]
  return(dbTab)
}
