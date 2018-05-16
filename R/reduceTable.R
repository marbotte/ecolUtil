reduceTable <-
function(tab,sumRowMin=5,sumRowMax=NA,sumColMin=3,sumColMax=NA,uniq=T,trace=T)
{
if(trace){cat("start dim",dim(tab),"\n")}
nbPass<-0
suppression<-T
suppRowMin<-suppRowMax<-suppColMin<-suppColMax<-dupl<-F
while(suppression)
{
if(trace){
nbPass<-nbPass+1
cat("pass n° ",nbPass,"\n")
}
if(!is.na(sumRowMin)){suppRowMin<-rowSums(tab)<sumRowMin}
if(!is.na(sumRowMax)){suppRowMax<-rowSums(tab)>sumRowMax}
if(!is.na(sumColMin)){suppColMin<-colSums(tab)<sumColMin}
if(!is.na(sumColMax)){suppColMax<-colSums(tab)>sumColMax}
if(uniq){dupl<-duplicated(tab)}
if(as.logical(sum(suppRowMin,suppRowMax,suppColMin,suppColMax,dupl)))
{tab<-tab[(!suppRowMin)&(!suppRowMax)&(!dupl),(!suppColMin)&(!suppColMax)]
}else
{suppression<-F
}
}
if(trace){cat("final dim",dim(tab),"\n")}
return(tab)
}
