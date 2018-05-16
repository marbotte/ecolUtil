reduceTable <-
function(tab,sumRowMin=5,sumRowMax=NA,sumColMin=3,sumColMax=NA,uniq=T,trace=T)
{
if(trace){cat("start dim",dim(table),"\n")}
nbPass<-0
suppression<-T
suppRowMin<-suppRowMax<-suppColMin<-suppColMax<-dupl<-F
while(suppression)
{
if(trace){
nbPass<-nbPass+1
cat("pass n° ",nbPass,"\n")
}
if(!is.na(sumRowMin)){suppRowMin<-rowSums(table)<sumRowMin}
if(!is.na(sumRowMax)){suppRowMax<-rowSums(table)>sumRowMax}
if(!is.na(sumColMin)){suppColMin<-colSums(table)<sumColMin}
if(!is.na(sumColMax)){suppColMax<-colSums(table)>sumColMax}
if(uniq){dupl<-duplicated(table)}
if(as.logical(sum(suppRowMin,suppRowMax,suppColMin,suppColMax,dupl)))
{table<-table[(!suppRowMin)&(!suppRowMax)&(!dupl),(!suppColMin)&(!suppColMax)]
}else
{suppression<-F
}
}
if(trace){cat("final dim",dim(table),"\n")}
return(table)
}
