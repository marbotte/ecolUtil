dbTab2mat <-
function(dbTab,col_samplingUnits="SU",col_species="sp",col_content="abundance",empty=NA)#empty permet de savoir comment remplir les vides de la matrice, si cela n'est pas dÃ©fini par l'utilisateur, Ã§a dÃ©pendra du mode de la colonne col_content du tableau. NB, il peut y avoir des bugs si la colonne col_content et empty n'ont pas le mÃªme type
{
  COLS<-unique(as.character(dbTab[,col_species]))
  ROWS<-unique(as.character(dbTab[,col_samplingUnits]))
  arr.which<-matrix(NA,ncol=2,nrow=nrow(dbTab),dimnames=list(1:nrow(dbTab),c("row","col")))
  arr.which[,1]<-match(as.character(dbTab[,col_samplingUnits]),ROWS)
  arr.which[,2]<-match(as.character(dbTab[,col_species]),COLS)
  modeContent<-mode(dbTab[,col_content])
  if(is.na(empty)){empty<-switch(modeContent,character="",numeric=0,logical=F)}
  res<-matrix(empty,ncol=length(COLS),nrow=length(ROWS),dimnames=list(ROWS,COLS))
  res[arr.which]<-dbTab[,col_content]
  return(res)
}
