mat2dbTab <-
function(mat,checklist=F)
{
  W<-which(mat>0,arr.ind<-T)
	if(!checklist){
  dbTab<-data.frame(SU=rownames(mat)[W[,"row"]],sp=colnames(mat)[W[,"col"]],ab=mat[W])
	}else{
  dbTab<-data.frame(SU=rownames(mat)[W[,"row"]],sp=colnames(mat)[W[,"col"]])
	}
	numSU<-all(grepl("^[0-9]+$",dbTab$SU))
	if(numSU){dbTab$SU<-as.numeric(as.character(dbTab$SU))}
  dbTab<-dbTab[order(dbTab$SU,dbTab$sp),]
  return(dbTab)
}
