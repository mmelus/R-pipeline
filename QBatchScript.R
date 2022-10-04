#Gérer des données longues est plus facile :
ImportIntoTable <- function(){
  Headers <- t(Importdatatest[c(1:19), c(1)])
  Headers <- cbind("Treatment",Headers)
  Headers <- cbind("CellNum",Headers)
  CellTable <<- data.frame(matrix(ncol = 21, nrow= 0))
  for(row in 1:nrow(BatchQuentin)){
    obs <- BatchQuentin[row,]
    CellNum <-obs$Name
    Treatment <-0
    if(obs$Vars!="NA"){
      Treatment<- t(Importdatatest[obs$Vars, c(obs$ColS:obs$ColE)])
    } else {
      Treatment<- colnames(Importdatatest)[obs$ColS:obs$ColE]
    }
     
    donnes <- t(Importdatatest[obs$RowS:obs$RowE,obs$ColS:obs$ColE])
    
    donnes <- cbind(Treatment, donnes)
    donnes <- cbind(CellNum, donnes)
    row.names(donnes)<-c()
    colnames(donnes)<-Headers
    CellTable <<- rbind(donnes, CellTable, stringsAsFactors = FALSE)
  }
  cols.num <- c(names(CellTable)[3:21])
  CellTable[cols.num] <<- sapply(CellTable[cols.num],as.numeric)
  sapply(CellTable, class)
  View(CellTable)
}
ImportIntoTable()

##Get treatment by letter in CellNum
getTreatment<- function(letter){
  return(CellTable[grep(paste0(".",letter),CellTable$CellNum),])
}
##View treatment by letter in CellNum
viewTreatment <- function(letter){
  View(getTreatment(letter))
}

## Get treatment's frequency means
# getTreatment("A",,"") <- pour obtenir TLettre seulement
getTreatmentMean<- function(letter, rowCoor, rowName){
  tmean <- as.data.frame(colMeans(getTreatment(letter)[rowCoor,3:21]))
  colnames(tmean)<- c(paste0("Trait", letter,":", rowName))
  return(tmean)
}
##View treatment's frequency means
# viewTreatment("A",,"") <- pour obtenir TLettre seulement
viewTreatmentMean<- function(letter, rowCoor, rowName){
  View(getTreatmentMean(letter, rowCoor, rowName))
}

##Create table of multiple treatment's frequency means
#letterVector exemple = c("A","B","C")
#rowCNVector exemple = list("Cell1"= 3:9, "Cell2"=30:20, "Cell3"=15:20)
#le plus simple serait de definir letterVector et rowCNVector à part et ensuite 
# getTraitMeansTable(letterVector, rowCNVector)
getTraitMeansTable <- function(letterVector, rowCNVector){
  df <- data.frame(matrix(ncol =0,nrow = 19))
  for(letter in letterVector){
    for(i in 1:length(rowCNVector)){
      name <- names(rowCNVector)[i]
      rowCor <- rowCNVector[[name]]
      print(rowCor)
      df <- cbind(df, getTreatmentMean(letter, rowCor, name))  
    }
  }
  tot <-as.data.frame(rowMeans(df))
  colnames(tot)<-"Sampling Mean"
  df <- cbind(df, tot)
  return(df)
}

##View table of multiple treatment's frequency means
viewTraitMeansTable <- function(letterVector, rowCNVector){
  View(getTraitMeansTable(letterVector, rowCNVector))
}

#Transpose pour export
getTMeansTableForExport<- function(letterVector, rowCNVector){
  View(t(getTraitMeansTable(letterVector, rowCNVector)))
  return(t(getTraitMeansTable(letterVector, rowCNVector)))
}

######################EXEMPLES UTILISATION###############
#si tu veux donner le traitement à une fonction ou l'assigner à une variable
getTreatment("A")
#Si tu veux le voir tu fais
viewTreatment("A")

#Pour avoir la moyenne des frequences dans un traitement
getTreatmentMean("A",,"")
#Tu peux spécifier les rows à chopper de ce Traitement en spécifique
#Et donner un nom à ce sous groupe s'affichant en entête TA:Nom
getTreatmentMean("A",1:10,"Nom")
#Meme concepte pour le View.

#Explications en dessus. Meme chose pour le View
getTraitMeansTable(c("A","B","C"),list("Cell1"=1:5, "Cell2"=6:10, "Cell3"=11:15))
viewTraitMeansTable(c("A","B","C"),list("Cell1"=1:5, "Cell2"=6:10, "Cell3"=11:15))
#Prends le getTraitMeansTable fait un t() dessus.
#Affichera la table par la même occasion comme ça tu es sûr d'avoir ce que tu veux
getTMeansTableForExport(c("A","B","C"),list("Cell1"=1:5, "Cell2"=6:10, "Cell3"=11:15))
