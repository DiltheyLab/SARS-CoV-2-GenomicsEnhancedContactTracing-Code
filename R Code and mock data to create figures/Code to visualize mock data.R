## Set working directory

##Load packages
library(igraph)
library(readxl)
library(stringr)
library(tidyr)
pacman::p_load(lubridate)
library(showtext)

##Generate mock data

mockCaseIDs <- paste0("CVD2021-00",c(1:100) )
mockSeqIDs <- c(rep("", 30), paste0("Z0",c(1:70) ))
mockRegDate <- sample(nodes$Meldedatum[nodes$month %in% c("2021-11")], 100)
mockVacc<- sample(nodes$Impfung_Anzahl[nodes$month %in% c("2021-11")], 100)
mockVaccDate <- sample(nodes$Impfung_Datum[nodes$month %in% c("2021-11")], 100)
mockVaccComp <- sample(nodes$Impfung_Impfstoff[nodes$month %in% c("2021-11")], 100)
mockKP <- sample(nodes$Count_Kontaktpersonen, 100)
mockAbsVon <- mockRegDate
mockSymp <- sample(nodes$Symptome, 100)
mockLineage <- sample(nodes$Lineage[nodes$month %in% c("2021-11")], 100)
mockNs <- sample(c(1:100), 100, replace=T)

mock_nodes <- data.frame(CaseID= mockCaseIDs, SeqID= mockSeqIDs, Meldedatum=mockRegDate,
                         Impfung_Anzahl= mockVacc, Impfung_Datum=mockVaccDate, Impfung_Impfstoff=mockVaccComp,
                         Count_Kontaktpersonen=mockKP, Count_Kontaktpersonen_Duesseldorf= mockKP,
                         AbsonderungVon=mockAbsVon, Symptome="fever", Lineage=mockLineage, Ns=mockNs)
 
#write.csv2(mock_nodes, file="Mock_Nodes.csv", row.names=F)
 
## Make mock edge file
mock_nodes <- read.csv("/mock/Mock_Nodes.csv", sep=";")
idxCases <- mock_nodes[mock_nodes$Count_Kontaktpersonen_Duesseldorf > 0,]

## Average case sequencing rate
mock_nodes$Meldedatum2 <- substr(mock_nodes$Meldedatum, 1, 10)

mock_nodes$Meldedatum2 <- as.character(lapply(mock_nodes$Meldedatum2, function(x){ 
   y <- strsplit(x, " ")[[1]][[1]]
   y <- gsub("\\.", "-",y)
   as.character(y)
} ))

mock_nodes$Meldedatum2 <- dmy(mock_nodes$Meldedatum2)

mock_nodes2 <- mock_nodes[mock_nodes$SeqID != "",]


##make KP connections
KPlist <- list()

i <- 1
for(i in 1:nrow(idxCases)){
   
   caseID <- idxCases$CaseID[i]
   nKP <-   idxCases$Count_Kontaktpersonen_Duesseldorf[i]
   
   dfKP <- data.frame(CaseID_Index = caseID, CaseID_Kontaktperson= sample(mock_nodes$CaseID, nKP))
   KPlist[[i]]  <- dfKP
}

library(data.table)
KPlist <- rbindlist(KPlist)
KPlist$Typ <- "SurvnetKontaktperson"
KPlist$Kontaktperson_Ort <- paste0("cpCVD2022-", c(1:nrow(KPlist)))
KPlist$KontaktpersonenID <- "Duesseldorf"
KPlist$DatumsDifferenz <- 0

KPlist$date1 <- mock_nodes$Meldedatum2[match(KPlist$CaseID_Index, mock_nodes$CaseID)]
KPlist$date2 <- mock_nodes$Meldedatum2[match(KPlist$CaseID_Kontaktperson, mock_nodes$CaseID)]

KPlist$DatumsDifferenz <- as.numeric(KPlist$date1 - KPlist$date2 )

KPlist$date1 <- NULL
KPlist$date2 <- NULL

KPlist$Genetic_Distance <- ""
KPlist$Genetic_Distance[KPlist$CaseID_Index %in% mock_nodes2$CaseID & KPlist$CaseID_Kontaktperson %in% mock_nodes2$CaseID] <- "-"

#sum(abs(KPlist$DatumsDifferenz[KPlist$Genetic_Distance == "-"]) <= 14)
 

##make sameAddress connections
SAlist <- list()
SAlist2<- list()

i <- 1
for(i in 1:nrow(mock_nodes)){
   
   caseID <- mock_nodes$CaseID[i]
   houseHoldCon <- sample(c(0,1,2,3), 1, prob=c(.1, .2,.3,.4))
   nKPs <- mock_nodes$Count_Kontaktpersonen_Duesseldorf[i]

   
   if(nKPs != 0) {
      
      dfKP2 <- data.frame(CaseID_Index = caseID,
                         CaseID_Kontaktperson= KPlist$CaseID_Kontaktperson[KPlist$CaseID_Index == caseID])
      
      SAlist2[[i]]  <- dfKP2
   } 
   
   if(houseHoldCon != 0) {
      
      dfKP <- data.frame(CaseID_Index = caseID, CaseID_Kontaktperson= sample(mock_nodes$CaseID, houseHoldCon))
      
      SAlist[[i]]  <- dfKP
   } 
   

}

library(data.table)
SAlist <- rbindlist(SAlist)
SAlist2 <- rbindlist(SAlist2)
## Select 6/8 of KPs as same address
SAlist2 <- SAlist2[sample(1:nrow(SAlist2), round(nrow(SAlist2)*0.75) ),]

SAlist <- rbind(SAlist, SAlist2)
SAlist <- unique(SAlist)

SAlist$Typ <- "SelbeAdresse"
SAlist$Kontaktperson_Ort <- ""
SAlist$KontaktpersonenID <- ""
SAlist$DatumsDifferenz <- 0

SAlist$date1 <- mock_nodes$Meldedatum2[match(SAlist$CaseID_Index, mock_nodes$CaseID)]
SAlist$date2 <- mock_nodes$Meldedatum2[match(SAlist$CaseID_Kontaktperson, mock_nodes$CaseID)]

SAlist$DatumsDifferenz <- as.numeric(SAlist$date1 - SAlist$date2 )

SAlist$date1 <- NULL
SAlist$date2 <- NULL

SAlist$Genetic_Distance <- ""
SAlist$Genetic_Distance[SAlist$CaseID_Index %in% mock_nodes2$CaseID & SAlist$CaseID_Kontaktperson %in% mock_nodes2$CaseID] <- "-"

SAlist2 <- SAlist[sample(1:nrow(SAlist), round(nrow(SAlist)*0.50) ),]
SAlist2$Typ <- "SelbeAdresseNachname"


### make angestecktBei make it manually beacuse only 5 edges to generate, 3 same add, 1 KP, 1 new
# SurvnetAngestecktBei
 
mock_edge <- rbind(KPlist, SAlist)

## write.csv2(mock_edge, file= "mock_edge.csv", row.names=F)
## Add 5 AngestecktBei manually

mock_edge <- read.csv("/mock/Mock_edge.csv", sep=";")

### Add 50% sameAddressSameName

mock_edge_SA <- mock_edge[mock_edge$Typ == "SelbeAdresse",]
mock_edge_SA2 <- mock_edge_SA[sample(1:nrow(mock_edge_SA), round(nrow(mock_edge_SA)*0.5) ),]
mock_edge_SA2$Typ <-   "SelbeAdresseNachname"

mock_edge <- rbind(mock_edge, mock_edge_SA2)

write.csv2(mock_edge, file= "Mock_edge.csv", row.names=F)

nodes <- mock_nodes
edge <- mock_edge

edgesGraph <- edge

## Average case sequencing rate
nodes$Meldedatum2 <- substr(nodes$Meldedatum, 1, 10)

nodes$Meldedatum2 <- as.character(lapply(nodes$Meldedatum2, function(x){ 
   y <- strsplit(x, " ")[[1]][[1]]
   y <- gsub("\\.", "-",y)
   as.character(y)
} ))

nodes$Meldedatum2 <- dmy(nodes$Meldedatum2)

nodes$month <- substr(nodes$Meldedatum2,1,7 )
months <- c( "2021-02", "2021-03", "2021-04", "2021-05",
             "2021-06", "2021-07", "2021-08", "2021-09", "2021-10",
             "2021-11", "2021-12" )
nodes <- nodes[nodes$month %in% months,]

nodes2 <- nodes
nodes2 <- nodes2[nodes2$CaseID %in% nodes$CaseID,]
nodes2$Meldedatum2 <- nodes$Meldedatum2[match(nodes2$CaseID, nodes$CaseID)]
nodes2 <- nodes2[nodes2$SeqID != "",]
nodes2 <- na.omit(nodes2)

getCaseID <- function(column){
   
   test <- column[column != "NA"]
   test <- na.omit(test)
   
   if(length(test) > 0){
      test <-  strsplit(test, "or")
      test <- lapply(test, function(x) {strsplit(x, ",")})
      test <- unlist(test)
      test <- strsplit(test, "\\n")
      test <- unlist(test)
      test <- strsplit(test, " ")
      test <- unlist(test)
      test <- strsplit(test, ":")
      test <- unlist(test)
      test <- strsplit(test, "'")
      test <- unlist(test)
      test <- gsub("\\(", "", test)
      test <- gsub("\\)", "", test)
      test <- gsub(" ", "", test)
      t2 <- data.frame(name=test)
      t2$start <- substr(t2$name, 1, 3)
      t2 <- t2[t2$start == "CVD",]
      
      return(t2$name)
   }else{
      
      return(column)
   }
}


getGeneticDistance <-function(seqID1, seqID2){
   
   cur_dm <- dm[which(dm$X == seqID2), which(names(dm) == seqID1) ]
   
   return(as.numeric(cur_dm))
   
}

edge2 <- edge[edge$CaseID_Index %in% nodes$CaseID | edge$CaseID_Kontaktperson %in% nodes$CaseID ,]

### Modify graph table for easy lookup
edgesGraph$index1 <- as.character(lapply(1:nrow(edgesGraph), function(x){
   
   i1 <- edgesGraph$CaseID_Index[x]
   i2 <- edgesGraph$CaseID_Kontaktperson[x]
   
   ids <- sort(c(i1, i2))
   ids[1]
   
}))

edgesGraph$index2 <- as.character(lapply(1:nrow(edgesGraph), function(x){
   
   i1 <- edgesGraph$CaseID_Index[x]
   i2 <- edgesGraph$CaseID_Kontaktperson[x]
   
   ids <- sort(c(i1, i2))
   ids[2]
   
}))

edgesGraph$CaseID_Index <- NULL
edgesGraph$CaseID_Kontaktperson <- NULL

edgesGraph$indexSearch <- paste(edgesGraph$index1, edgesGraph$index2)

edge2$index1 <- as.character(lapply(1:nrow(edge2), function(x){
   
   i1 <- edge2$CaseID_Index[x]
   i2 <- edge2$CaseID_Kontaktperson[x]
   
   ids <- sort(c(i1, i2))
   ids[1]
   
}))

edge2$index2 <- as.character(lapply(1:nrow(edge2), function(x){
   
   i1 <- edge2$CaseID_Index[x]
   i2 <- edge2$CaseID_Kontaktperson[x]
   
   ids <- sort(c(i1, i2))
   ids[2]
   
}))

edge2$indexSearch <- paste(edge2$index1, edge2$index2)
edge2$indexSearch <- paste(edge2$indexSearch, edge2$Typ)

edgesGraph$indexSearch <- paste(edgesGraph$indexSearch, edgesGraph$Typ)


edges3 <- edge2[edge2$indexSearch %in% edgesGraph$indexSearch,]
edge2 <- edge2[edge2$indexSearch %in% edgesGraph$indexSearch,]

edge2$indexSearch2 <- paste(edge2$index1, edge2$index2)
edge2$KontextSearchID <- paste(edge2$CaseID_Index,",", edge2$Kontaktperson_Ort)

edge2$context <- ""
edge2$context[edge2$Typ == "SurvnetKontaktperson" &
                 edge2$indexSearch2 %in%  edge2$indexSearch2[edge2$Typ == "SelbeAdresse"]] <- "Private household"

write.csv2(edge2, file="Mock_edge2_context.csv", row.names=F)

##Mock outbreaks

outbreaks <- read.csv("mock/Mock_Outbreak_to_context.csv", sep=";")
outbreaks$Outbreak.ID <- c("CVD-DUS-ALT-2021-0001", "CVD-DUS-ALT-2021-0002",
                           "CVD-DUS-SCHULE-2021-0001", "CVD-DUS-SCHULE-2021-0002")
 
#Aktenzeichen	AlterBerechnet	AusbruchInfo_Aktenzeichen	AusbruchInfo_GuidRecord	AusbruchInfo_NameGA	Geschlecht	Meldedatum

## Outbreak cases mock
outbreakDF <- list()

for(i in outbreaks$Outbreak.ID){
   
   ids <- data.frame(Aktenzeichen=sample(nodes$CaseID, 10))
   ids$AlterBerechnet <- sample(c(20:70), 10)
   
   ids$AusbruchInfo_Aktenzeichen <- paste0("Outbreak2021", i)
   ids$AusbruchInfo_GuidRecord <- paste0("6a2f47f3-", i)
   ids$AusbruchInfo_NameGA <- i
   ids$Geschlecht <- "weiblich"
   ids$Meldedatum <- nodes$Meldedatum2[match(ids$Aktenzeichen, nodes$CaseID)]
   
   outbreakDF[[i]] <- ids
}

outbreakDF <- rbindlist(outbreakDF)

write.csv2(outbreakDF, file= "Mock_outbreak_cases.csv", row.names=F)

edgesGraph <- edge2

#save(getCaseID, getGeneticDistance, nodes, nodes2, edge, edge2, file="Mock_data.Rdata")

##Generate mock dm
load("Mock_data.Rdata")
 
ids <- c("X",nodes2$SeqID)

# Create empty matrix
mat <- matrix(sample(c(0:3), length(ids)^2, replace=T), nrow = length(ids), ncol = length(ids))

rownames(mat) <- ids
colnames(mat) <- ids

matDF <- data.frame(mat)
matDF$X <- row.names(matDF)
matDF  <- matDF[row.names(matDF) != "X",]
row.names(matDF) <- NULL

# get all ID columns (everything except X)
id_cols <- setdiff(colnames(matDF), "X")

# loop through each ID column
for (col in id_cols) {
   matDF[[col]][ matDF$X == col ] <- 0
}

dm2 <- matDF

dm <- dm2

 

#save(edgesGraph, dm, getCaseID, getGeneticDistance, nodes, nodes2, edge, edge2, file="Mock_data.Rdata")

#write.csv2(dm, file="Mock_DM.csv", row.names = F)

load("Mock_data.Rdata")
## Laod new distance matrix
 
 
## Generate outbreak tables

## make new outbreak caseID association table
 newOutbreaks <- read.csv("/mock/Mock_outbreak_cases.csv", sep=";")

newOutbreaks$Erkrankungsbeginn <- NULL
newOutbreaks <- na.omit(newOutbreaks)

allOutbreaks <- data.frame(outbreakID = unique(newOutbreaks$AusbruchInfo_NameGA))

allOutbreaks$cases2021 <- 0
allOutbreaks$cases2021 <- as.numeric(lapply(allOutbreaks$outbreakID, function(x){
   
   cases <- unique(newOutbreaks$Aktenzeichen[newOutbreaks$AusbruchInfo_NameGA == x])
   
   casesDates <- nodes$Meldedatum2[nodes$CaseID %in% cases]
   casesDates <- substr(casesDates, 1, 4)
   
   sum(casesDates == "2021")
   
}))


allOutbreaks$sumCases <- 0
allOutbreaks$sumCases <- as.numeric(lapply(allOutbreaks$outbreakID, function(x){
   
   cases <- unique(newOutbreaks$Aktenzeichen[newOutbreaks$AusbruchInfo_NameGA == x])
   length(cases)
   
   
}))


allOutbreaks$casesDiff <- allOutbreaks$cases2021 - allOutbreaks$sumCases

## get number of outbreaks with cases in 2021
sum(allOutbreaks$cases2021 > 0)

nrow(newOutbreaks[newOutbreaks$AusbruchInfo_NameGA %in% allOutbreaks$outbreakID[allOutbreaks$cases2021 > 0],])

allOutbreaks2 <- allOutbreaks[allOutbreaks$cases2021 > 0,]
allOutbreaks3 <- allOutbreaks2[allOutbreaks2$casesDiff == 0,]

## cases overlapping years
nrow(allOutbreaks2) - nrow(allOutbreaks3)

allOutbreaks4 <- allOutbreaks3[allOutbreaks3$cases2021 > 1,]

nrow(allOutbreaks3) - nrow(allOutbreaks4)

global_size <- 15

## Correct spelling mistakes found in Survnet database
outbreak <- newOutbreaks[newOutbreaks$AusbruchInfo_NameGA %in% allOutbreaks4$outbreakID,] 
outbreak$type <- gsub("CVD-DÜS-", "", outbreak$AusbruchInfo_NameGA)
outbreak$type <- gsub("CVD-DUS-", "", outbreak$type)
outbreak$type <- gsub("CVD2021-10144-2220-DU-", "", outbreak$type)
outbreak$type <- gsub("CVD-Dus-", "", outbreak$type)
outbreak$type <- gsub("COV-DÜS-", "", outbreak$type)
outbreak$type <- gsub("CVD-", "", outbreak$type)

outbreak$type2 <- as.character(lapply(outbreak$type, function(x){
   
   strsplit(x, "-")[[1]][[1]]
   
}))

outbreak$year <- "2021"

## Gather outbreak inforamtion
outbreak$type2[outbreak$type2 == "KRH"] <- "KRANKENHAUS"
outbreak$type2[outbreak$type2 == "KH"] <- "KRANKENHAUS"
outbreak$type2[outbreak$type2 == "Heim"] <- "HEIM"
outbreak$type2[outbreak$type2 == "Arbeit"] <- "ARBEIT"
outbreak$type2[outbreak$type2 == "RKN"] <- "KRANKENHAUS"
outbreak$type2[outbreak$type2 == "ALT"] <- "ALT"
outbreak$type2[outbreak$type2 == "BAR/ALTSTADT"] <- "ALTSTADT"
outbreak$type2[outbreak$type2 == "CAFFE/ALTSTADT2021"] <- "ALTSTADT"
outbreak$type2[outbreak$type2 == "COCTAILBAR/ALTSTADT"] <- "ALTSTADT"
outbreak$type2[outbreak$type2 == "Abschiedsparty"] <- "ABSCHIEDSPARTY"
outbreak$type2[outbreak$type2 == "ClubTor3"] <- "CLUBTOR3"
outbreak$type2[outbreak$type2 == "ME"] <- "KRANKENHAUS"

outbreak$type3 <- outbreak$type2
outbreak$type3[outbreak$type2 == "ABSCHIEDSPARTY"] <- "Nightlife"
outbreak$type3[outbreak$type2 == "ALTSTADT"] <-  "Nightlife"
outbreak$type3[outbreak$type2 == "BAR"] <- "Nightlife"
outbreak$type3[outbreak$type2 == "BRAUEREI"] <-  "Nightlife"
outbreak$type3[outbreak$type2 == "CLUBTOR3"] <- "Nightlife"
outbreak$type3[outbreak$type2 == "FEIER"] <-  "Nightlife"
outbreak$type3[outbreak$type2 == "VERANSTALTUNG"] <- "Nightlife"
outbreak$type3[outbreak$type2 == "ARBEIT"] <- "Work"
outbreak$type3[outbreak$type2 == "PRIV"] <- "Private"
outbreak$type3[outbreak$type2 == "FREIZEIT"] <- "Private"
outbreak$type3[outbreak$type2 == "SPORT"] <- "Private"
outbreak$type3[outbreak$type2 == "JVA"] <- "Other"
outbreak$type3[outbreak$type2 == "ALT"] <- "Care home"
outbreak$type3[outbreak$type2 == "KITA"] <- "Daycare"
outbreak$type3[outbreak$type2 == "JUGENDHILFE"] <- "Daycare"
outbreak$type3[outbreak$type2 == "KRANKENHAUS"] <- "Hospital"
outbreak$type3[outbreak$type2 == "SCHULE"] <- "School"
outbreak$type3[outbreak$type2 %in% c("W","RP", "OG","PB")] <- "Other"
outbreak$type3[outbreak$type3 == "Daycare"] <- "Kindergarten"
outbreak$type3[outbreak$type3 == "Private"] <- "Recreational context"

table(outbreak$type3)

 
##### Table creation

## Generate outbreak suppl table
outbreakTableNamePath <- "/mock/Mock_Moritz-Table Final.xlsx"

## Load outbreak table of medical student
eva <- read_excel(outbreakTableNamePath, sheet = "Tabelle2")

eva2 <- eva
eva2 <- eva2[!is.na(eva2$Investigator),]
eva <- eva2
rm(eva2)

## Function to retrieve CaseIDs from manually entered text fields
getCaseID <- function(column){
   
   test <- column[column != "NA"]
   test <- na.omit(test)
   
   if(length(test) > 0 & any(test != "")){
      test <-  strsplit(test, "or")
      test <- lapply(test, function(x) {strsplit(x, ",")})
      test <- unlist(test)
      test <- strsplit(test, "\\n")
      test <- unlist(test)
      test <- strsplit(test, " ")
      test <- unlist(test)
      test <- strsplit(test, ":")
      test <- unlist(test)
      test <- strsplit(test, "'")
      test <- unlist(test)
      test <- gsub("\\(", "", test)
      test <- gsub("\\)", "", test)
      test <- gsub(" ", "", test)
      t2 <- data.frame(name=test)
      t2$start <- substr(t2$name, 1, 3)
      t2 <- t2[t2$start == "CVD",]
      t2$name <- gsub("\r", "",  t2$name)
      
      return(t2$name)
   }else{
      
      return(column[column != ""])
   }
}
 
outbreaks <- outbreak

## Create output table layout
outFilter <- data.frame(
   ini= eva$`Outbreak ID`,
   outbreakID="" 
)

## Correct outbreak name spelling mistakes
eva$`Outbreak ID` <- gsub("COV", "CVD", eva$`Outbreak ID`)
outbreaks$AusbruchInfo_NameGA <- gsub("COV", "CVD", outbreaks$AusbruchInfo_NameGA)
outbreaks$AusbruchInfo_NameGA <- gsub("Heim", "HEIM", outbreaks$AusbruchInfo_NameGA )

outbreaks$MeldedatumDate <- nodes$Meldedatum2[match(outbreaks$Aktenzeichen, nodes$CaseID)]

## only analyse outbreaks that are part of our outbreak list
for(outbrakeID in c(1:nrow(eva))){
   
   ## Get ID (column outbreakID)
   print(outbrakeID)
   id <- eva$`Outbreak ID`[outbrakeID]
   id <- strsplit(id, " ")[[1]][[1]]  ## Remove any additions to ID
   
   ##If ID is not part of ID list check outbreak-case ID 
   if(!id %in% outbreaks$AusbruchInfo_NameGA){
      id <- unique(outbreaks$AusbruchInfo_NameGA[regexpr(id, outbreaks$AusbruchInfo_Aktenzeichen) != -1])
      if(length(id) > 1) id <- na.omit(id)
      if(length(id)==0 ) next()
      if(is.na(id)) next()
   }
   
   ## If outbreak ID still not found, break
   if(!id %in% outbreaks$AusbruchInfo_NameGA){
      id <- unique(outbreaks$AusbruchInfo_NameGA[regexpr(id, outbreaks$AusbruchInfo_Aktenzeichen) != -1])
   }
   
   outFilter$outbreakID[outbrakeID] <- id
   
}

outFilter <- outFilter[outFilter$outbreakID != "",]
eva <- eva[eva$`Outbreak ID` %in% outFilter$ini,]

eva$`Outbreak ID` <-  outFilter$outbreakID[match(eva$`Outbreak ID`, outFilter$ini)]

out <- data.frame(
   ini= eva$`Outbreak ID`,
   outbreakID=eva$`Outbreak ID`)


missingOut <- unique(outbreaks$AusbruchInfo_NameGA)
missingOut <- missingOut[!missingOut %in% eva$`Outbreak ID`]

outbreakIDs <- c(out$outbreakID, missingOut )

## Create output table layout
out <- data.frame(
   ini= outbreakIDs,
   outbreakID=outbreakIDs, 
   month="",
   firstDate="", 
   lastDate="",  
   Duration_days=0,
   outbreakType="", 
   n_casesTotal=0,   
   n_casesDuesseldorf=0, 
   n_sequencedCases=0,
   sequencedCases_clonal=0,
   sequencedCases_clonal_IDs ="",
   misassignedCases=0,  # treshold 2 distance  
   misassignedCasesCaseIDs="",
   certainly_misassignedCases=0,  # treshold 5 distance
   genLinkedCommunityCases=0,  # genetically identical to at least one outbreak case 
   genLinkedCommunityCasesnoEdge=0,  # genetically identical to at least one outbreak case, with no edge
   genLinkedCommunityCasesCaseIDs="",  # genetically identical to at least one outbreak case 
   genLinkedCommunityCasesnoEdgeCaseIDs="",  # genetically identical to at least one outbreak case, with no edge
   
   ## structured contact person columns
   n_contactPersonsDuesseldorf=0,
   n_positiveContactPersonsDuesseldorf=0,
   n_positiveContactPersonsDuesseldorfRegistered=0,
   n_positiveContactPersonsDuesseldorfNotRegistered=0,
   n_positiveContactPersonsDuesseldorfNotRegisteredCaseIDs="",
   n_positiveContactPersonsDuesseldorfNotRegisteredLinks=0,
   n_positiveContactPersonsDuesseldorfNotRegisteredSequenced=0,
   n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinked=0,   
   n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenNOTLinked=0,   
   n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs="",   
   n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenNOTLinkedCaseID="",  
   n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissed=0,  
   n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissedCaseID="",  
   
   ## count infected By records
   n_communityCasesInfectedByOutbreakCasesNotRegistered=0,
   n_OutbreakCasesInfectedBycommunityCasesNotRegistered=0,
   n_communityCasesInfectedByOutbreakCasesNotRegisteredCaseIDs="",
   n_OutbreakCasesInfectedBycommunityCasesNotRegisteredCaseIDs="",
   n_NotRegisteredInfectedByLinksTotal=0,
   n_NotRegisteredInfectedByLinksTotalSequenced=0,
   n_NotRegisteredInfectedByLinksTotalSequencedGenLinked=0,
   n_NotRegisteredInfectedByLinksTotalSequencedGenNotLinked=0,
   n_NotRegisteredInfectedByLinksTotalSequencedGenLinkedCaseIDs="",
   n_NotRegisteredInfectedByLinksTotalSequencedGenNotLinkedCaseIDs="",
   
   
   ## same address information
   n_positiveSameAddressCases=0,
   n_positiveSameAddressCasesRegistered=0,
   n_positiveSameAddressCasesNotRegistered=0,
   n_positiveSameAddressCasesNotRegisteredCaseIDs="",
   n_positiveSameAddressCasesNotRegisteredSequenced=0,
   n_positiveSameAddressCasesNotRegisteredSequencedGenLinked=0,  
   n_positiveSameAddressCasesNotRegisteredSequencedGenNOTLinked=0,  
   n_positiveSameAddressCasesNotRegisteredSequencedGenLinkedCaseIDs="",  
   n_positiveSameAddressCasesNotRegisteredSequencedGenNOTLinkedCaseIDs="", 
   n_positiveSameAddressCasesNotRegisteredSequencedGenLinkedNotContact=0,  #same as above but no contact
   
   ## non-structured contact person columns
   nonstructuredContactsAnalysed=0,  
   n_nonstructuredContactPersonsDuesseldorf=0, 
   n_nonstructuredPositiveContactPersonsDuesseldorf=0, 
   n_nonstructuredPositiveContactPersonsDuesseldorfRegistered=0, 
   n_nonstructuredPositiveContactPersonsDuesseldorfNotRegistered=0, 
   n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredCaseIDs="", 
   n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequenced=0, 
   n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinked=0,    
   n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenNOTLinked =0,
   n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs="",   
   n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenNOTLinkedCaseIDs ="",
   n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissed=0,
   n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissedCaseID="", 
   
   externalClusterLinksInverstigated=0,  
   externalGenLinksInverstigated=0, 
   otherGenLinkedOutbreakIDs="", 
   outbreakGenLinkGrammatic="", 
   genLinkedOutbreakIDs="",  
   genLinkedNonContactPersonCaseIDs="",   
   genLinkedNonContactPersonCaseIDCount=0, 
   genLinkedNonContactPersonCaseIDMissed="",
   genLinkedNonContactPersonCaseIDMissedCount=0,
   otherwiseLinkedCaseIDs="",
   linkedNonContactPersonNonSequencedNonOutbreakCaseIDs="",  
   linkedNonContactPersonNonSequencedNonOutbreakCaseIDsCount=0,  
   linkedNonContactPersonNonSequencedNonOutbreakMissed="", 
   linkedNonContactPersonNonSequencedNonOutbreakMissedCount=0, 
   linkedNonContactPersonNonSequencedNonOutbreakCaseIDsCountSameAdressLinked=0,  
   linkedNonContactPersonNonSequencedNonOutbreakMissedCountSameAdressLinked=0 ,
   casesLinkedByEpidemiologicalDataCaseIDs="", 
   erroneouslyAssignedCasesCaseIDs=""  
   
)

#test specific outbreak
outbrakeID <- 2
options(warn=2)

eva[eva == "0.0"] <- "0"

for(outbrakeID in c(1:nrow(out))){
   
   ## Get ID (column outbreakID)
   print(outbrakeID)
   id <- out$outbreakID[outbrakeID] 
   
   ## Get all outbreak caseIDs from health department data
   cur_outbreack <- outbreaks[outbreaks$AusbruchInfo_NameGA == id,]
   
   ##Check if outbreak was studied in detail
   detailed <- T
   if(is.na(eva$`Düsseldorf Cases, total`[outbrakeID])) detailed <- F
   
   if(nrow(cur_outbreack) != 0){
      
      ## Get earlies outbreak month
      dates <- substr(cur_outbreack$MeldedatumDate,6,7)
      dateTab <- table(dates)
      out$month[outbrakeID] <-  min(names(dateTab)) 
      
      out$firstDate[outbrakeID] <-  as.character(min(cur_outbreack$MeldedatumDate) )
      out$lastDate[outbrakeID] <-  as.character(max(cur_outbreack$MeldedatumDate) )
      out$Duration_days[outbrakeID] <- as.numeric( max(cur_outbreack$MeldedatumDate) - min(cur_outbreack$MeldedatumDate) )
      
      ## Get outbreak Type (column outbreakType)
      out$outbreakType[outbrakeID] <- eva$Category[outbrakeID]
      if(detailed == F) out$outbreakType[outbrakeID] <- unique(cur_outbreack$type3)
      
      ## Get total case count (columns n_casesTotal, n_casesDuesseldorf)
      ## Report cases from student table
      out$n_casesTotal[outbrakeID] <- eva$`Düsseldorf Cases, total`[outbrakeID] + eva$`Non-Düsseldorf Cases, total`[outbrakeID] 
      if(detailed == F) out$n_casesTotal[outbrakeID] <- nrow(cur_outbreack)
      
      out$n_casesDuesseldorf[outbrakeID] <-  eva$`Düsseldorf Cases, total`[outbrakeID]
      if(detailed == T){
         if(nrow(cur_outbreack) != eva$`Düsseldorf Cases, total`[outbrakeID] ) out$n_casesDuesseldorf[outbrakeID] <- nrow(cur_outbreack) # unsere daten rein
      }
      if(detailed == F) out$n_casesDuesseldorf[outbrakeID] <- nrow(cur_outbreack)
      
      ## Get count of sequenced cases (column n_sequencedCases)
      out$n_sequencedCases[outbrakeID] <- nrow(nodes2[nodes2$CaseID %in% cur_outbreack$Aktenzeichen,])
      
      ## Calculate, if sequenced cases can be connected via MST with genetic distance < 2 (column sequencedCases_clonal)
      ## Get cases and check gen distances between samples
      outlist <- data.frame(name=id, clonal="distance not available")
      
      if(exists("connected")) rm(connected)
      
      cases <- nodes[nodes$CaseID %in% outbreaks$Aktenzeichen[outbreaks$AusbruchInfo_NameGA == outlist$name[1]],]
      cases <- cases[cases$SeqID != "",]
      
      c1 <- strsplit(cases$SeqID, ",")
      
      if(length(c1)>0){
         
         cc <- expand.grid(c1, c1)
         cc2 <- expand.grid(cases$CaseID, cases$CaseID)
         
         cc$case1 <- cc2$Var1
         cc$case2 <- cc2$Var2
         
         cc$Var1 <- as.character(cc$Var1)
         cc$Var2 <- as.character(cc$Var2)
         
         cc <- cc[cc$Var1 != cc$Var2,]
         
         if(nrow(cc) != 0){
            
            cc$var1_inDM <- 0
            cc$var2_inDM <- 0
            
            cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
            cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
            
            cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
            
            if(nrow(cc) != 0){
               
               cc$dist <- lapply(1:nrow(cc), function(x){
                  
                  getGeneticDistance(cc$Var1[x], cc$Var2[x])
                  
               })
               
               ## remove large dist
               
               cc4 <- data.frame(case1=cc$case1 ,case2= cc$case2, dist=as.numeric(cc$dist) )
               cc4$casePair <- as.character(lapply(seq_len(nrow(cc4)), function(x){
                  
                  paste(sort(c(cc4$case1[x],cc4$case2[x])), collapse= ",")
                  
               }))
               
               
               cc5 <- data.frame(pair=cc4$casePair, dist=cc4$dist)
               cc5 <- unique(cc5)
               cc5 <- cc5[cc5$dist %in% c(1,0),]
               
               if(nrow(cc5) != 0){
                  cc5$case1 <- as.character(lapply(cc5$pair, function(x){
                     
                     strsplit(x, ",")[[1]][[1]]
                     
                  }))
                  
                  cc5$case2 <- as.character(lapply(cc5$pair, function(x){
                     
                     strsplit(x, ",")[[1]][[2]]
                     
                  }))
                  
                  cluster <- list()
                  for(x in unique(cc5$case1)){
                     
                     cluster[[x]] <- cc5$case2[cc5$case1 == x]
                     
                  }
                  
                  connected <- c(cluster[[1]], names(cluster)[1])
                  
                  ## Expand radius until nothing changes
                  status <- "continue"
                  while(status== "continue"){
                     
                     connected_cluster <- lapply(names(cluster), function(x){
                        
                        y <- cluster[[x]]
                        
                        if(any(y %in% connected)) return(c(x, y))
                        
                     })
                     
                     connected2 <-  unique(c(connected, unlist(connected_cluster)))
                     
                     if(length(connected) == length(connected2)){
                        
                        status <- "stop"
                        
                     } else{
                        
                        connected <- connected2 
                     }
                     
                  }
                  
                  ##save clonal cases for later
                  connectedIDsOutbreak <- connected
                  
                  # outbreak entry
                  if(length(connected) == length(cases$CaseID)){
                     
                     outlist$clonal[1] <- "clonal"
                     out$misassignedCases[outbrakeID] <- 0
                     
                  }else{
                     
                     outlist$clonal[1] <- "not clonal"
                     out$misassignedCases[outbrakeID] <- length(cases$CaseID) - length(connected)
                     out$misassignedCasesCaseIDs[outbrakeID] <- paste(cases$CaseID[!cases$CaseID %in% connected], collapse = ",")
                  }
                  
               } else{
                  
                  outlist$clonal[1] <- "not clonal"
                  out$misassignedCases[outbrakeID] <- nrow(cases)
                  out$misassignedCasesCaseIDs[outbrakeID] <- paste(cases$CaseID, collapse = ",")
                  
               }
               
               
            } else{ 
               outlist$clonal[1] <- "not available"
               out$misassignedCases[outbrakeID] <- "not available"}
            
         }else{ 
            outlist$clonal[1] <- "not available"
            out$misassignedCases[outbrakeID] <- "not available"}
         
      }
      
      out$sequencedCases_clonal[outbrakeID] <- outlist$clonal[1]
      
      if(exists("connected")) out$sequencedCases_clonal_IDs[outbrakeID]  <- paste(connected, collapse = ",")
      
      if((out$sequencedCases_clonal[outbrakeID] == "not clonal" & exists("connected"))|
         (out$sequencedCases_clonal[outbrakeID] == "not clonal" & nrow(cases) == 2)){
         
         ## Count misassigned cases with genDistance > 5 (column misassignedCases)
         outlist$clonal <- "NA"
         
         cases <- nodes[nodes$CaseID %in% outbreaks$Aktenzeichen[outbreaks$AusbruchInfo_NameGA == outlist$name[1]],]
         cases <- cases[cases$SeqID != "",]
         if(nrow(cases) > 2) cases <- cases[!cases$CaseID %in% connected, ]
         
         if(exists("connected") | (nrow(cases) == 2)){
            
            if(exists("connected")) casesConnected <- nodes[match(connected, nodes$CaseID) ,]
            if (nrow(cases) == 2) casesConnected <- nodes[match(cases$CaseID, nodes$CaseID) ,] 
            
            casesConnected <- casesConnected[casesConnected$SeqID != "",]
            
            c1 <- strsplit(cases$SeqID, ",")
            c2 <- strsplit(casesConnected$SeqID, ",")
            
            if(length(c1)>0){
               
               cc <- expand.grid(c1, c2)
               cc2 <- expand.grid(cases$CaseID, casesConnected$CaseID)
               
               cc$case1 <- cc2$Var1
               cc$case2 <- cc2$Var2
               
               cc$Var1 <- as.character(cc$Var1)
               cc$Var2 <- as.character(cc$Var2)
               
               cc <- cc[cc$Var1 != cc$Var2,]
               
               if(nrow(cc) != 0){
                  
                  cc$var1_inDM <- 0
                  cc$var2_inDM <- 0
                  
                  cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
                  cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
                  
                  cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
                  
                  if(nrow(cc) != 0){
                     
                     cc$dist <- lapply(1:nrow(cc), function(x){
                        
                        getGeneticDistance(cc$Var1[x], cc$Var2[x])
                        
                     })
                     
                     ## remove large dist
                     cc4 <- data.frame(case1=cc$case1 ,case2= cc$case2, dist=as.numeric(cc$dist) )
                     cc4$casePair <- as.character(lapply(seq_len(nrow(cc4)), function(x){
                        
                        paste(sort(c(cc4$case1[x],cc4$case2[x])), collapse= ",")
                        
                     }))
                     
                     ids <- data.frame(ids=unique(cc4$case1))
                     
                     ids$anyLinksLower5 <- as.numeric(lapply(ids$ids, function(x){
                        
                        curConnect <- cc4[cc4$case1 == x,]
                        sum(curConnect$dist <= 5)
                        
                     }))
                     
                     outlist$clonal[1] <- nrow(ids[ids$anyLinksLower5 == 0,])
                     
                  } else{ outlist$clonal[1] <- "NA" }
                  
               }else{ outlist$clonal[1] <- "NA" }
               
               
               
               
            }
            
         }
         
         
         out$certainly_misassignedCases[outbrakeID] <- outlist$clonal[1]
         if(out$n_sequencedCases[outbrakeID] == 2 & out$certainly_misassignedCases[outbrakeID] == 2){
            out$certainly_misassignedCases[outbrakeID] <- 1
         }
      }
      
      
      cases <- nodes[nodes$CaseID %in% outbreaks$Aktenzeichen[outbreaks$AusbruchInfo_NameGA == outlist$name[1]],]
      cases <- cases[cases$SeqID != "",]

      connected <- cases$CaseID
      
      ## Get genetically linked community cases, that are not part of another outbreak (column genLinkedCommunityCases)
      outlist <- data.frame(name= id, clonal="", n_clonal=0)
      
      i <- 1
      
      conCases <- list()
      
      ## only search for community cases related to the genetic cluster
      if(exists("connected")){
         
         cases <- nodes[nodes$CaseID %in% connected,]
         cases <- cases[cases$SeqID != "",]
         
         
         for(everyCase in  seq_len(nrow(cases))){
            
            #everyCase <- 1
            curCase <- cases[everyCase,]
            
            ##Get best case sequence
            c1 <- curCase$SeqID
            
            ##Subet all sequenced cases by date
            caseDate <- substr( curCase$Meldedatum, 1, 10)
            caseDate <- strsplit(caseDate, " ")[[1]][[1]]
            caseDate <- as.character(gsub("\\.", "-",caseDate))
            caseDate <- dmy(caseDate)
            
            nodes2$dateDiff <- 0
            nodes2$dateDiff <- as.numeric(lapply(nodes2$Meldedatum2, function(x){
               
               abs(as.numeric(x - caseDate))
               
            }))
            
            allCases_subset <- nodes2[nodes2$dateDiff <= 14,]
            allCases_subset <- allCases_subset[!allCases_subset$CaseID %in% cases$CaseID,]
            
            allCases_subset <- allCases_subset[!allCases_subset$CaseID %in% outbreaks$Aktenzeichen,]
            
            ## if cases in similar date, calculate genetic distance
            if(nrow(allCases_subset) > 0){
               
               ##Get best case sequence
               c1All <- allCases_subset$SeqID
               
               cc <- expand.grid(c1, c1All)
               cc2 <- expand.grid(curCase$CaseID, allCases_subset$CaseID)
               
               cc$case1 <- as.character(cc2$Var1)
               cc$case2 <- as.character(cc2$Var2)
               
               cc$Var1 <- as.character(cc$Var1)
               cc$Var2 <- as.character(cc$Var2)
               
               cc <- cc[cc$Var1 != cc$Var2,]
               
               cc$var1_inDM <- 0
               cc$var2_inDM <- 0
               
               cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
               cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
               
               cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
               
               if(nrow(cc) != 0){
                  
                  cc$dist <- lapply(1:nrow(cc), function(x){
                     
                     getGeneticDistance(cc$Var1[x], cc$Var2[x])
                     
                  })
                  
                  conCases[[curCase$CaseID]] <-   cc$case2[cc$dist <= 1]
                  
                  
               }
            }
         }
      }
      
      
      outlist$clonal[i] <- paste(unique(unlist(conCases)), collapse=",")
      outlist$n_clonal[i] <- length(unique(unlist(conCases)))
      
      out$genLinkedCommunityCases[outbrakeID] <- outlist$n_clonal[1]
      
      out$genLinkedCommunityCasesCaseIDs[outbrakeID] <-  paste(getCaseID(unique(unlist(conCases))), collapse=",")
      
      #column genLinkedCommunityCasesnoEdge
      genLinkedCommunityCasesnoEdge <- strsplit(outlist$clonal[1], ",")[[1]]
      genLinkedCommunityCasesnoEdge <- getCaseID(genLinkedCommunityCasesnoEdge)
      
      outbreakEdges <- edge2[edge2$CaseID_Index %in% cur_outbreack$Aktenzeichen,]
      outbreakEdges <- outbreakEdges[outbreakEdges$indexSearch %in% edgesGraph$indexSearch,]
      
      if(length(genLinkedCommunityCasesnoEdge) > 0){
         if(genLinkedCommunityCasesnoEdge[1] != ""){
            
            genLinkedCommunityCasesnoEdge <- genLinkedCommunityCasesnoEdge[!genLinkedCommunityCasesnoEdge %in% outbreakEdges$CaseID_Kontaktperson ]
            
         }}
      
      out$genLinkedCommunityCasesnoEdge[outbrakeID] <- length(genLinkedCommunityCasesnoEdge != "" & genLinkedCommunityCasesnoEdge != "NA")
      out$genLinkedCommunityCasesnoEdgeCaseIDs[outbrakeID] <-  paste(getCaseID(unique(genLinkedCommunityCasesnoEdge[genLinkedCommunityCasesnoEdge != "" & genLinkedCommunityCasesnoEdge != "NA"])), collapse=",")
      
      
      
      ## Contact person columns
      ## column n_contactPersonsDuesseldorf
      out$n_contactPersonsDuesseldorf[outbrakeID] <- sum(nodes$Count_Kontaktpersonen_Duesseldorf[nodes$CaseID %in% cur_outbreack$Aktenzeichen])
      
      ## column n_positiveContactPersonsDuesseldorf and n_positiveContactPersonsDuesseldorfRegistered 
      ## and n_positiveContactPersonsDuesseldorfNotRegistered
      posContacts <- edge2[edge2$CaseID_Index %in% cur_outbreack$Aktenzeichen & edge2$Typ == "SurvnetKontaktperson",]
      posContacts <- posContacts[posContacts$indexSearch %in% edgesGraph$indexSearch,]
      
      out$n_positiveContactPersonsDuesseldorf[outbrakeID] <- length(unique(posContacts$Kontaktperson_Ort))
      out$n_positiveContactPersonsDuesseldorfRegistered[outbrakeID] <- sum(cur_outbreack$Aktenzeichen %in% posContacts$CaseID_Kontaktperson)
      out$n_positiveContactPersonsDuesseldorfNotRegistered[outbrakeID] <- sum(!unique(posContacts$CaseID_Kontaktperson) %in% cur_outbreack$Aktenzeichen)
      out$n_positiveContactPersonsDuesseldorfNotRegisteredCaseIDs[outbrakeID] <- paste(getCaseID(unique(posContacts$CaseID_Kontaktperson[!posContacts$CaseID_Kontaktperson %in% cur_outbreack$Aktenzeichen])), collapse=",")
      
      out$n_positiveContactPersonsDuesseldorfNotRegisteredLinks[outbrakeID] <- sum(!posContacts$CaseID_Kontaktperson %in% cur_outbreack$Aktenzeichen)
      
      ## From positive contacts, that were not registered for the outbreak, how many were sequenced
      posContacts_seq <- posContacts[!posContacts$CaseID_Kontaktperson %in% cur_outbreack$Aktenzeichen,]
      posContacts_seq <- posContacts_seq[posContacts_seq$CaseID_Kontaktperson %in% nodes2$CaseID,]
      
      out$n_positiveContactPersonsDuesseldorfNotRegisteredSequenced[outbrakeID] <- length(unique(posContacts_seq$Kontaktperson_Ort))
      
      posContacts_seq <-  nodes2[nodes2$CaseID %in% posContacts_seq$CaseID_Kontaktperson,]
      
      ## From positive contacts, that were not registered for the outbreak, how many were sequenced and are genetically linked to at least one outbreak case
      ## column n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinked
      outlist <- data.frame(name=id, clonal="NA", n_clonal=0)
      
      if(nrow(posContacts_seq > 0)){
         
         cases <- nodes[nodes$CaseID %in% posContacts_seq$CaseID,]
         cases <- cases[cases$SeqID != "",]
         
         conCases <- list()
         
         for(everyCase in  seq_len(nrow(cases))){
            
            #everyCase <- 1
            curCase <- cases[everyCase,]
            
            ##Get best case sequence
            c1 <- curCase$SeqID
            
            allCases_subset <- nodes2[nodes2$CaseID %in% "",]
            if(exists("connected")) allCases_subset <- nodes2[nodes2$CaseID %in% connected,]
            
            if(nrow(allCases_subset) > 0){
               
               ##Get best case sequence
               c1All <- allCases_subset$SeqID
               
               cc <- expand.grid(c1, c1All)
               cc2 <- expand.grid(curCase$CaseID, allCases_subset$CaseID)
               
               cc$case1 <- as.character(cc2$Var1)
               cc$case2 <- as.character(cc2$Var2)
               
               cc$Var1 <- as.character(cc$Var1)
               cc$Var2 <- as.character(cc$Var2)
               
               cc <- cc[cc$Var1 != cc$Var2,]
               
               cc$var1_inDM <- 0
               cc$var2_inDM <- 0
               
               cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
               cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
               
               cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
               
               if(nrow(cc) != 0){
                  
                  cc$dist <- lapply(1:nrow(cc), function(x){
                     
                     getGeneticDistance(cc$Var1[x], cc$Var2[x])
                     
                  })
                  
                  if(any(cc$dist <= 1)) conCases[[curCase$CaseID]] <-   curCase$CaseID
                  
                  
               }
            }
         }
         
         
         outlist$clonal[1] <- paste(unique(unlist(conCases)), collapse=",")
         outlist$n_clonal[1] <- length(unique(unlist(conCases)))
      }
      
      out$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinked[outbrakeID] <- outlist$n_clonal[1]
      
      n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs <- strsplit(outlist$clonal[1], ",")[[1]]
      
      
      out$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs[outbrakeID] <- paste(getCaseID(unique(n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs)), collapse=",")
      
      
      ## From positive contacts, that were not registered for the outbreak, how many were sequenced and are genetically NOT linked to at least one outbreak case
      outlist <- data.frame(name=id, clonal="NA", n_clonal=0)
      
      if(nrow(posContacts_seq > 0)){
         
         cases <- nodes[nodes$CaseID %in% posContacts_seq$CaseID,]
         cases <- cases[cases$SeqID != "",]
         
         conCases <- list()
         
         for(everyCase in  seq_len(nrow(cases))){
            
            #everyCase <- 1
            curCase <- cases[everyCase,]
            
            ##Get best case sequence
            c1 <- curCase$SeqID
            
            allCases_subset <- nodes2[nodes2$CaseID %in% "",]
            if(exists("connected")) allCases_subset <- nodes2[nodes2$CaseID %in% connected,]
            
            if(nrow(allCases_subset) > 0){
               
               ##Get best case sequence
               c1All <- allCases_subset$SeqID
               
               cc <- expand.grid(c1, c1All)
               cc2 <- expand.grid(curCase$CaseID, allCases_subset$CaseID)
               
               cc$case1 <- as.character(cc2$Var1)
               cc$case2 <- as.character(cc2$Var2)
               
               cc$Var1 <- as.character(cc$Var1)
               cc$Var2 <- as.character(cc$Var2)
               
               cc <- cc[cc$Var1 != cc$Var2,]
               
               cc$var1_inDM <- 0
               cc$var2_inDM <- 0
               
               cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
               cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
               
               cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
               
               if(nrow(cc) != 0){
                  
                  cc$dist <- lapply(1:nrow(cc), function(x){
                     
                     getGeneticDistance(cc$Var1[x], cc$Var2[x])
                     
                  })
                  
                  if(all(cc$dist > 1)) conCases[[curCase$CaseID]] <-   curCase$CaseID
                  
                  
               }
            }
         }
         
         
         outlist$clonal[1] <- paste(unique(unlist(conCases)), collapse=",")
         outlist$n_clonal[1] <- length(unique(unlist(conCases)))
      }
      
      out$n_positiveSameAddressCasesNotRegisteredSequencedGenNOTLinked[outbrakeID] <- outlist$n_clonal[1]
      
      out$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenNOTLinkedCaseID[outbrakeID] <- paste(getCaseID(unique(strsplit(outlist$clonal[1], ",")[[1]])), collapse=",")
      
      
      
      ## From positive contacts, that were not registered for the outbreak, how many were 
      ## sequenced and are genetically linked to at least one outbreak case and were missed 
      ## in the outbreak according to the medical students
      ## column n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissed
      missed1 <- getCaseID(eva$`(Case IDs for the previous column)...33`[outbrakeID])
      missed1 <- unique(missed1)
      
      missed1 <- missed1[missed1 %in% posContacts_seq$CaseID]
      
      out$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissed[outbrakeID] <- length(missed1)
      out$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissedCaseID[outbrakeID] <- paste(getCaseID(unique(missed1)), collapse=",")
      
      
      
      
      
      ## InfectedBy columns
      ## column communityCasesInfectedByOutbreakCasesNotRegistered
      InfectedBy <- edge2[edge2$CaseID_Index %in% cur_outbreack$Aktenzeichen & edge2$Typ == "SurvnetAngestecktBei",]
      InfectedBy <- InfectedBy[InfectedBy$indexSearch %in% edgesGraph$indexSearch,]
      InfectedBy <- InfectedBy[!InfectedBy$CaseID_Kontaktperson %in% outbreaks$Aktenzeichen,]
      out$n_OutbreakCasesInfectedBycommunityCasesNotRegistered[outbrakeID] <- length(unique(InfectedBy$CaseID_Kontaktperson))
      if(length(unique(InfectedBy$CaseID_Kontaktperson))!= 0) out$n_OutbreakCasesInfectedBycommunityCasesNotRegisteredCaseIDs[outbrakeID] <- paste(getCaseID(unique(InfectedBy$CaseID_Kontaktperson)), collapse=",")  
      
      CommunityInfectedBy <- edge2[edge2$CaseID_Kontaktperson %in% cur_outbreack$Aktenzeichen & edge2$Typ == "SurvnetAngestecktBei",]
      CommunityInfectedBy <- CommunityInfectedBy[CommunityInfectedBy$indexSearch %in% edgesGraph$indexSearch,]
      CommunityInfectedBy <- CommunityInfectedBy[!CommunityInfectedBy$CaseID_Index %in% outbreaks$Aktenzeichen,]
      out$n_communityCasesInfectedByOutbreakCasesNotRegistered[outbrakeID] <- length(unique(CommunityInfectedBy$CaseID_Index))
      if(length(unique(CommunityInfectedBy$CaseID_Index))!= 0)  out$n_communityCasesInfectedByOutbreakCasesNotRegisteredCaseIDs[outbrakeID] <- paste(getCaseID(unique(CommunityInfectedBy$CaseID_Index)), collapse=",") 
      
      
      
      #switch caseIDs for downstream analysis
      CommunityInfectedBy2 <- CommunityInfectedBy
      CommunityInfectedBy2$CaseID_Index <- CommunityInfectedBy2$CaseID_Kontaktperson
      CommunityInfectedBy2$CaseID_Kontaktperson <- CommunityInfectedBy$CaseID_Index
      
      ## Combine both for further analysis
      InfectedBy <- rbind(InfectedBy, CommunityInfectedBy2)
      
      out$n_NotRegisteredInfectedByLinksTotal[outbrakeID] <- sum(!unique(InfectedBy$CaseID_Kontaktperson) %in% cur_outbreack$Aktenzeichen)
      
      ## From positive contacts, that were not registered for the outbreak, how many were sequenced
      InfectedBy_seq <- InfectedBy[!InfectedBy$CaseID_Kontaktperson %in% cur_outbreack$Aktenzeichen,]
      InfectedBy_seq <- InfectedBy_seq[InfectedBy_seq$CaseID_Kontaktperson %in% nodes2$CaseID,]
      
      out$n_NotRegisteredInfectedByLinksTotalSequenced[outbrakeID] <- length(unique(InfectedBy_seq$CaseID_Kontaktperson))
      
      InfectedBy_seq <-  nodes2[nodes2$CaseID %in% InfectedBy_seq$CaseID_Kontaktperson,]
      
      ## From positive contacts, that were not registered for the outbreak, how many were sequenced and are genetically linked to at least one outbreak case
      ## column n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinked
      outlist <- data.frame(name=id, clonal="NA", n_clonal=0)
      
      if(nrow(InfectedBy_seq > 0)){
         
         cases <- nodes[nodes$CaseID %in% InfectedBy_seq$CaseID,]
         cases <- cases[cases$SeqID != "",]
         
         conCases <- list()
         
         for(everyCase in  seq_len(nrow(cases))){
            
            #everyCase <- 1
            curCase <- cases[everyCase,]
            
            ##Get best case sequence
            c1 <- curCase$SeqID
            
            allCases_subset <- nodes2[nodes2$CaseID %in% "",]
            if(exists("connected")) allCases_subset <- nodes2[nodes2$CaseID %in% connected,]
            
            if(nrow(allCases_subset) > 0){
               
               ##Get best case sequence
               c1All <- allCases_subset$SeqID
               
               cc <- expand.grid(c1, c1All)
               cc2 <- expand.grid(curCase$CaseID, allCases_subset$CaseID)
               
               cc$case1 <- as.character(cc2$Var1)
               cc$case2 <- as.character(cc2$Var2)
               
               cc$Var1 <- as.character(cc$Var1)
               cc$Var2 <- as.character(cc$Var2)
               
               cc <- cc[cc$Var1 != cc$Var2,]
               
               cc$var1_inDM <- 0
               cc$var2_inDM <- 0
               
               cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
               cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
               
               cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
               
               if(nrow(cc) != 0){
                  
                  cc$dist <- lapply(1:nrow(cc), function(x){
                     
                     getGeneticDistance(cc$Var1[x], cc$Var2[x])
                     
                  })
                  
                  if(any(cc$dist <= 1)) conCases[[curCase$CaseID]] <-   curCase$CaseID
                  
                  
               }
            }
         }
         
         
         outlist$clonal[1] <- paste(unique(unlist(conCases)), collapse=",")
         outlist$n_clonal[1] <- length(unique(unlist(conCases)))
      }
      
      out$n_NotRegisteredInfectedByLinksTotalSequencedGenLinked[outbrakeID] <- outlist$n_clonal[1]
      out$n_NotRegisteredInfectedByLinksTotalSequencedGenLinkedCaseIDs[outbrakeID] <- outlist$clonal[1]
      
      
      ## From positive contacts, that were not registered for the outbreak, how many were sequenced and are genetically NOT linked to at least one outbreak case
      outlist <- data.frame(name=id, clonal="NA", n_clonal=0)
      
      if(nrow(InfectedBy_seq > 0)){
         
         cases <- nodes[nodes$CaseID %in% InfectedBy_seq$CaseID,]
         cases <- cases[cases$SeqID != "",]
         
         conCases <- list()
         
         for(everyCase in  seq_len(nrow(cases))){
            
            #everyCase <- 1
            curCase <- cases[everyCase,]
            
            ##Get best case sequence
            c1 <- curCase$SeqID
            
            allCases_subset <- nodes2[nodes2$CaseID %in% "",]
            if(exists("connected")) allCases_subset <- nodes2[nodes2$CaseID %in% connected,]
            
            if(nrow(allCases_subset) > 0){
               
               ##Get best case sequence
               c1All <- allCases_subset$SeqID
               
               cc <- expand.grid(c1, c1All)
               cc2 <- expand.grid(curCase$CaseID, allCases_subset$CaseID)
               
               cc$case1 <- as.character(cc2$Var1)
               cc$case2 <- as.character(cc2$Var2)
               
               cc$Var1 <- as.character(cc$Var1)
               cc$Var2 <- as.character(cc$Var2)
               
               cc <- cc[cc$Var1 != cc$Var2,]
               
               cc$var1_inDM <- 0
               cc$var2_inDM <- 0
               
               cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
               cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
               
               cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
               
               if(nrow(cc) != 0){
                  
                  cc$dist <- lapply(1:nrow(cc), function(x){
                     
                     getGeneticDistance(cc$Var1[x], cc$Var2[x])
                     
                  })
                  
                  if(all(cc$dist > 1)) conCases[[curCase$CaseID]] <-   curCase$CaseID
                  
                  
               }
            }
         }
         
         
         outlist$clonal[1] <- paste(unique(unlist(conCases)), collapse=",")
         outlist$n_clonal[1] <- length(unique(unlist(conCases)))
      }
      
      out$n_NotRegisteredInfectedByLinksTotalSequencedGenNotLinked[outbrakeID] <- outlist$n_clonal[1]
      out$n_NotRegisteredInfectedByLinksTotalSequencedGenNotLinkedCaseIDs[outbrakeID]  <- outlist$clonal[1]
      
      
      
      
      
      
      ## Same address columns
      
      ## column n_positiveSameAddressCases and n_positiveSameAddressCasesRegistered 
      ## and n_positiveSameAddressCasesNotRegistered and n_positiveSameAddressCasesNotRegisteredLinks
      edge2$reg <- 0
      edge2$otherCase <- ""
      posContacts <- edge2[(edge2$CaseID_Index %in% cur_outbreack$Aktenzeichen & edge2$Typ == "SelbeAdresse")|
                              edge2$CaseID_Kontaktperson %in% cur_outbreack$Aktenzeichen & edge2$Typ == "SelbeAdresse",]
      posContacts <- posContacts[posContacts$indexSearch %in% edgesGraph$indexSearch,]
      
      ##find registered same address cases
      posContacts$reg[posContacts$CaseID_Index %in% cur_outbreack$Aktenzeichen &
                         posContacts$CaseID_Kontaktperson %in% cur_outbreack$Aktenzeichen] <- 1
      
      out$n_positiveSameAddressCases[outbrakeID] <- nrow(posContacts)
      out$n_positiveSameAddressCasesRegistered[outbrakeID] <- sum(posContacts$reg)
      out$n_positiveSameAddressCasesNotRegistered[outbrakeID] <- sum(posContacts$reg==0)
      
      
      ## From same address cases, that were not registered for the outbreak, how many were sequenced
      # posContacts$otherCase <- ""
      posContacts$otherCase[posContacts$CaseID_Index %in% cur_outbreack$Aktenzeichen] <- posContacts$CaseID_Kontaktperson[posContacts$CaseID_Index %in% cur_outbreack$Aktenzeichen]
      posContacts$otherCase[posContacts$CaseID_Kontaktperson %in% cur_outbreack$Aktenzeichen] <- posContacts$CaseID_Index[posContacts$CaseID_Kontaktperson %in% cur_outbreack$Aktenzeichen]
      
      posContacts_seq <- posContacts[posContacts$reg == 0,]
      out$n_positiveSameAddressCasesNotRegisteredCaseIDs[outbrakeID] <- paste(unique(posContacts_seq$otherCase[posContacts_seq$reg == 0]), collapse=",")
      
      
      posContacts_seq <- nodes2[nodes2$CaseID %in% posContacts_seq$otherCase,]
      out$n_positiveSameAddressCasesNotRegisteredSequenced[outbrakeID] <- nrow(posContacts_seq)
      
      ## From positive same address cases, that were not registered for the outbreak, how many were sequenced and are genetically linked to at least one outbreak case
      ## column n_positiveSameAddressCasesNotRegisteredSequencedGenLinked
      outlist <- data.frame(name=id, clonal="NA", n_clonal=0)
      
      if(nrow(posContacts_seq > 0)){
         
         cases <- nodes[nodes$CaseID %in% posContacts_seq$CaseID,]
         cases <- cases[cases$SeqID != "",]
         
         conCases <- list()
         
         for(everyCase in  seq_len(nrow(cases))){
            
            #everyCase <- 1
            curCase <- cases[everyCase,]
            
            ##Get best case sequence
            c1 <- curCase$SeqID
            
            allCases_subset <- nodes2[nodes2$CaseID %in% "",]
            if(exists("connected")) allCases_subset <- nodes2[nodes2$CaseID %in% connected,]
            
            
            if(nrow(allCases_subset) > 0){
               
               ##Get best case sequence
               c1All <- allCases_subset$SeqID
               
               cc <- expand.grid(c1, c1All)
               cc2 <- expand.grid(curCase$CaseID, allCases_subset$CaseID)
               
               cc$case1 <- as.character(cc2$Var1)
               cc$case2 <- as.character(cc2$Var2)
               
               cc$Var1 <- as.character(cc$Var1)
               cc$Var2 <- as.character(cc$Var2)
               
               cc <- cc[cc$Var1 != cc$Var2,]
               
               cc$var1_inDM <- 0
               cc$var2_inDM <- 0
               
               cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
               cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
               
               cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
               
               if(nrow(cc) != 0){
                  
                  cc$dist <- lapply(1:nrow(cc), function(x){
                     
                     getGeneticDistance(cc$Var1[x], cc$Var2[x])
                     
                  })
                  
                  if(any(cc$dist <= 1)) conCases[[curCase$CaseID]] <-   curCase$CaseID
                  
                  
               }
            }
         }
         
         
         outlist$clonal[1] <- paste(unique(unlist(conCases)), collapse=",")
         outlist$n_clonal[1] <- length(unique(unlist(conCases)))
      }
      
      out$n_positiveSameAddressCasesNotRegisteredSequencedGenLinked[outbrakeID] <- outlist$n_clonal[1]
      out$n_positiveSameAddressCasesNotRegisteredSequencedGenLinkedCaseIDs[outbrakeID] <- paste(getCaseID(unique(strsplit(outlist$clonal[1],",")[[1]])), collapse=",")
      
      
      
      nocont <- strsplit(outlist$clonal[1], ",")[[1]]
      nocont <- nocont[!nocont%in% n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs]
      
      out$n_positiveSameAddressCasesNotRegisteredSequencedGenLinkedNotContact[outbrakeID] <- length(nocont)
      
      
      ## From positive same address cases, that were not registered for the outbreak, how many were sequenced and are genetically NOT linked to at least one outbreak case
      ## column n_positiveSameAddressCasesNotRegisteredSequencedGenLinked
      outlist <- data.frame(name=id, clonal="NA", n_clonal=0)
      
      if(nrow(posContacts_seq > 0)){
         
         cases <- nodes[nodes$CaseID %in% posContacts_seq$CaseID,]
         cases <- cases[cases$SeqID != "",]
         
         conCases <- list()
         
         for(everyCase in  seq_len(nrow(cases))){
            
            #everyCase <- 1
            curCase <- cases[everyCase,]
            
            ##Get best case sequence
            c1 <- curCase$SeqID
            
            allCases_subset <- nodes2[nodes2$CaseID %in% "",]
            if(exists("connected")) allCases_subset <- nodes2[nodes2$CaseID %in% connected,]
            
            
            if(nrow(allCases_subset) > 0){
               
               ##Get best case sequence
               c1All <- allCases_subset$SeqID
               
               cc <- expand.grid(c1, c1All)
               cc2 <- expand.grid(curCase$CaseID, allCases_subset$CaseID)
               
               cc$case1 <- as.character(cc2$Var1)
               cc$case2 <- as.character(cc2$Var2)
               
               cc$Var1 <- as.character(cc$Var1)
               cc$Var2 <- as.character(cc$Var2)
               
               cc <- cc[cc$Var1 != cc$Var2,]
               
               cc$var1_inDM <- 0
               cc$var2_inDM <- 0
               
               cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
               cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
               
               cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
               
               if(nrow(cc) != 0){
                  
                  cc$dist <- lapply(1:nrow(cc), function(x){
                     
                     getGeneticDistance(cc$Var1[x], cc$Var2[x])
                     
                  })
                  
                  if(all(cc$dist > 1)) conCases[[curCase$CaseID]] <-   curCase$CaseID
                  
                  
               }
            }
         }
         
         
         outlist$clonal[1] <- paste(unique(unlist(conCases)), collapse=",")
         outlist$n_clonal[1] <- length(unique(unlist(conCases)))
      }
      
      out$n_positiveSameAddressCasesNotRegisteredSequencedGenNOTLinked[outbrakeID] <- outlist$n_clonal[1]
      out$n_positiveSameAddressCasesNotRegisteredSequencedGenNOTLinkedCaseIDs[outbrakeID] <- paste(getCaseID(unique(strsplit(outlist$clonal[1],",")[[1]])), collapse=",")
      
      
      ## column about whether unstructured contacts were analysed for the outbreak (column nonstructuredContactsAnalysed)
      out$nonstructuredContactsAnalysed[outbrakeID] <- eva$`Analysis of non-structured contact information based on free-text case data fields carried out (0 or 1)`[outbrakeID]
      if(is.na(out$nonstructuredContactsAnalysed[outbrakeID])) out$nonstructuredContactsAnalysed[outbrakeID] <- 0
      if(out$nonstructuredContactsAnalysed[outbrakeID] == "NA") out$nonstructuredContactsAnalysed[outbrakeID] <- 0
      
      if(out$nonstructuredContactsAnalysed[outbrakeID] != 0){
         
         ## Non-structered Contact person columns
         ## column n_nonstructuredContactPersonsDuesseldorf
         out$n_nonstructuredContactPersonsDuesseldorf[outbrakeID] <- as.numeric(eva$`Non-structured contacts of Düsseldorf cases (count)`[outbrakeID])
         
         ## column n_nonstructuredPositiveContactPersonsDuesseldorf
         #nonstructuredPositiveContacts <- eva$`Non-structured contacts of Düsseldorf cases, tested positive (count)`[outbrakeID]
         nonstructuredPositiveContacts <- getCaseID(eva$"(Case IDs of previous column)...39"[outbrakeID])
         out$n_nonstructuredPositiveContactPersonsDuesseldorf[outbrakeID] <- length(nonstructuredPositiveContacts)
         if(nonstructuredPositiveContacts[1] == "NA" | is.na(nonstructuredPositiveContacts[1])) out$n_nonstructuredPositiveContactPersonsDuesseldorf[outbrakeID] <- 0
         
         ## column n_nonstructuredPositiveContactPersonsDuesseldorfRegistered and n_nonstructuredPositiveContactPersonsDuesseldorfNotRegistered
         out$n_nonstructuredPositiveContactPersonsDuesseldorfRegistered[outbrakeID]  <- sum(cur_outbreack$Aktenzeichen %in% nonstructuredPositiveContacts)
         out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegistered[outbrakeID] <- sum(!nonstructuredPositiveContacts %in% cur_outbreack$Aktenzeichen)
         out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredCaseIDs[outbrakeID] <- paste(getCaseID(nonstructuredPositiveContacts[!nonstructuredPositiveContacts %in% cur_outbreack$Aktenzeichen]), collapse=",")
         
         if(nonstructuredPositiveContacts[1] == "NA" | is.na(nonstructuredPositiveContacts[1])) out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegistered[outbrakeID] <- 0
         
         ## From positive nonstructured contacts, that were not registered for the outbreak, how many were sequenced
         ## column n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequenced
         posContacts_seq <- nonstructuredPositiveContacts[!nonstructuredPositiveContacts %in% cur_outbreack$Aktenzeichen]
         posContacts_seq <- nodes2[nodes2$CaseID %in% posContacts_seq,]
         out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequenced[outbrakeID] <- nrow(posContacts_seq)
         
         ## From positive nonstructured contacts, that were not registered for the outbreak, 
         ## how many were sequenced and are genetically linked to at least one outbreak case
         ## column n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinked
         outlist <- data.frame(name=id, clonal="NA", n_clonal=0)
         
         if(nrow(posContacts_seq > 0)){
            
            cases <- nodes[nodes$CaseID %in% posContacts_seq$CaseID,]
            cases <- cases[cases$SeqID != "",]
            
            conCases <- list()
            
            for(everyCase in  seq_len(nrow(cases))){
               
               #everyCase <- 1
               curCase <- cases[everyCase,]
               
               ##Get best case sequence
               c1 <- curCase$SeqID
               
               allCases_subset <- nodes2[nodes2$CaseID %in% "",]
               if(exists("connected")) allCases_subset <- nodes2[nodes2$CaseID %in% connected,]
               
               if(nrow(allCases_subset) > 0){
                  
                  ##Get best case sequence
                  c1All <- allCases_subset$SeqID
                  
                  cc <- expand.grid(c1, c1All)
                  cc2 <- expand.grid(curCase$CaseID, allCases_subset$CaseID)
                  
                  cc$case1 <- as.character(cc2$Var1)
                  cc$case2 <- as.character(cc2$Var2)
                  
                  cc$Var1 <- as.character(cc$Var1)
                  cc$Var2 <- as.character(cc$Var2)
                  
                  cc <- cc[cc$Var1 != cc$Var2,]
                  
                  cc$var1_inDM <- 0
                  cc$var2_inDM <- 0
                  
                  cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
                  cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
                  
                  cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
                  
                  if(nrow(cc) != 0){
                     
                     cc$dist <- lapply(1:nrow(cc), function(x){
                        
                        getGeneticDistance(cc$Var1[x], cc$Var2[x])
                        
                     })
                     
                     if(any(cc$dist <= 1)) conCases[[curCase$CaseID]] <-   curCase$CaseID
                     
                     
                  }
               }
            }
            
            
            outlist$clonal[1] <- paste(unique(unlist(conCases)), collapse=",")
            outlist$n_clonal[1] <- length(unique(unlist(conCases)))
         }
         
         out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinked[outbrakeID] <- outlist$n_clonal[1]
         
         out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs[outbrakeID] <- paste(unique(getCaseID(strsplit(outlist$clonal[1],",")[[1]])), collapse=",")
         
         
         ## From positive nonstructured contacts, that were not registered for the outbreak, 
         ## how many were sequenced and are NOT genetically linked to at least one outbreak case
         ## column n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinked
         outlist <- data.frame(name=id, clonal="NA", n_clonal=0)
         
         if(nrow(posContacts_seq > 0)){
            
            cases <- nodes[nodes$CaseID %in% posContacts_seq$CaseID,]
            cases <- cases[cases$SeqID != "",]
            
            conCases <- list()
            
            for(everyCase in  seq_len(nrow(cases))){
               
               #everyCase <- 1
               curCase <- cases[everyCase,]
               
               ##Get best case sequence
               c1 <- curCase$SeqID
               
               allCases_subset <- nodes2[nodes2$CaseID %in% "",]
               if(exists("connected")) allCases_subset <- nodes2[nodes2$CaseID %in% connected,]
               
               if(nrow(allCases_subset) > 0){
                  
                  ##Get best case sequence
                  c1All <- allCases_subset$SeqID
                  
                  cc <- expand.grid(c1, c1All)
                  cc2 <- expand.grid(curCase$CaseID, allCases_subset$CaseID)
                  
                  cc$case1 <- as.character(cc2$Var1)
                  cc$case2 <- as.character(cc2$Var2)
                  
                  cc$Var1 <- as.character(cc$Var1)
                  cc$Var2 <- as.character(cc$Var2)
                  
                  cc <- cc[cc$Var1 != cc$Var2,]
                  
                  cc$var1_inDM <- 0
                  cc$var2_inDM <- 0
                  
                  cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
                  cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
                  
                  cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
                  
                  if(nrow(cc) != 0){
                     
                     cc$dist <- lapply(1:nrow(cc), function(x){
                        
                        getGeneticDistance(cc$Var1[x], cc$Var2[x])
                        
                     })
                     
                     if(all(cc$dist > 1)) conCases[[curCase$CaseID]] <-   curCase$CaseID
                     
                     
                  }
               }
            }
            
            
            outlist$clonal[1] <- paste(unique(unlist(conCases)), collapse=",")
            outlist$n_clonal[1] <- length(unique(unlist(conCases)))
         }
         
         out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenNOTLinked[outbrakeID] <- outlist$n_clonal[1]
         out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenNOTLinkedCaseIDs[outbrakeID] <- paste(unique(getCaseID(strsplit(outlist$clonal[1],",")[[1]])), collapse=",")
         
         
         
         ## From positive nonstructured contacts, that were not registered for the outbreak, how many were 
         ## sequenced and are genetically linked to at least one outbreak case and were missed 
         ## in the outbreak according to the medical students
         ## column n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissed
         missed2 <- getCaseID(eva$`(Case IDs of previous column)...49`[outbrakeID])
         missed2 <- unique(missed2)
         
         ##Check genetic distance and no contact person 
         oubtreakKP <- edge2[edge2$CaseID_Index %in% cur_outbreack$Aktenzeichen & 
                                edge2$Typ == "SurvnetKontaktperson",]
         oubtreakKP <- oubtreakKP[oubtreakKP$indexSearch %in% edgesGraph$indexSearch,]
         
         
         missed2 <- missed2[!missed2 %in% oubtreakKP$CaseID_Kontaktperson]
         missed2 <- missed2[!missed2 %in% cur_outbreack$Aktenzeichen]
         
         out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissed[outbrakeID] <- length(missed2)
         out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissedCaseID[outbrakeID] <- paste(missed2, collapse=",")
      }else{
         
         out$n_nonstructuredContactPersonsDuesseldorf[outbrakeID] <- NA
         out$n_nonstructuredPositiveContactPersonsDuesseldorf[outbrakeID] <- NA
         out$n_nonstructuredPositiveContactPersonsDuesseldorfRegistered[outbrakeID]  <- NA
         out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegistered[outbrakeID] <- NA
         out$n_nonstructuredPositiveContactPersonsDuesseldorf[outbrakeID] <- NA
         out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequenced[outbrakeID] <- NA
         out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinked[outbrakeID] <-  NA
         out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissed[outbrakeID] <- NA
         out$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissedCaseID[outbrakeID] <- NA
         
      }
      
      
      
      ## Outbreak-external links investigated based on Genetic Clusters or genetic distance checks
      ## column externalClusterLinksInverstigated  and  externalGenLinksInverstigated
      out$externalClusterLinksInverstigated[outbrakeID] <- eva$`Outbreak-external links investigated based on Genetic Clusters (0 or 1)`[outbrakeID]
      out$externalGenLinksInverstigated[outbrakeID] <- eva$`Outbreak-external links investigated based on individual case genetic distance checks (0 or 1)`[outbrakeID]
      if(detailed == F) out$externalClusterLinksInverstigated[outbrakeID] <- 0
      if(detailed == F) out$externalGenLinksInverstigated[outbrakeID] <- 0
      
      ## Linked outbreak IDs, search for every outbreak ID within text field
      ## column otherGenLinkedOutbreakIDs
      linkedOutbreaks <- getCaseID(eva$`Linked to other outbreaks (IDs)`[outbrakeID])
      
      if(linkedOutbreaks[1] == "NA" | is.na(linkedOutbreaks[1])){
         
         out$otherGenLinkedOutbreakIDs[outbrakeID] <- ""
         
      }else{
         
         conected_outbreaks <- list()
         
         for(conOut in strsplit(linkedOutbreaks, ",")[[1]]){
            
            conOut <- gsub(" ", "", conOut)
            
            if(conOut == id) next()
            conOutCases <- nodes2[nodes2$CaseID %in% outbreaks$Aktenzeichen[outbreaks$AusbruchInfo_NameGA == conOut ],]
            
            ##Calculate distance between both outbreak cases
            outlist <- data.frame(name=id, clonal="NA", n_clonal=0)
            
            posContacts_seq <- nodes2[nodes2$CaseID %in% "",]
            if(exists("connected")) posContacts_seq <- nodes2[nodes2$CaseID %in% connected,]
            
            if(nrow(posContacts_seq > 0)){
               
               cases <- posContacts_seq
               conCases <- list()
               
               for(everyCase in  seq_len(nrow(cases))){
                  
                  #everyCase <- 1
                  curCase <- cases[everyCase,]
                  
                  ##Get best case sequence
                  c1 <- curCase$SeqID
                  allCases_subset <- conOutCases
                  
                  if(nrow(allCases_subset) > 0){
                     
                     ##Get best case sequence
                     c1All <- allCases_subset$SeqID
                     
                     cc <- expand.grid(c1, c1All)
                     cc2 <- expand.grid(curCase$CaseID, allCases_subset$CaseID)
                     
                     cc$case1 <- as.character(cc2$Var1)
                     cc$case2 <- as.character(cc2$Var2)
                     
                     cc$Var1 <- as.character(cc$Var1)
                     cc$Var2 <- as.character(cc$Var2)
                     
                     cc <- cc[cc$Var1 != cc$Var2,]
                     
                     cc$var1_inDM <- 0
                     cc$var2_inDM <- 0
                     
                     cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
                     cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
                     
                     cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
                     
                     if(nrow(cc) != 0){
                        
                        cc$dist <- lapply(1:nrow(cc), function(x){
                           
                           getGeneticDistance(cc$Var1[x], cc$Var2[x])
                           
                        })
                        
                        if(any(cc$dist <= 1)) conCases[[curCase$CaseID]] <-   curCase$CaseID
                        
                        
                     }
                  }
               }
               
               
               outlist$clonal[1] <- paste(unique(unlist(conCases)), collapse=",")
               outlist$n_clonal[1] <- length(unique(unlist(conCases)))
               if(outlist$n_clonal > 0) conected_outbreaks[[conOut]] <- conOut
            }
            
            
            
            
         }
         
         out$otherGenLinkedOutbreakIDs[outbrakeID] <- paste(unlist(conected_outbreaks), collapse = ", ")
      }
      
      
      
      ## Get the grammatic of outbreak links per outbreak, column outbreakGenLinkGrammatic
      outbreaksGramma <- eva$`Formal, grammar-based description of the identified links to other outbreaks (Grammatiken)`[outbrakeID]
      outbreaksGramma <- strsplit(outbreaksGramma, "\\\n")[[1]]
      out$outbreakGenLinkGrammatic[outbrakeID]  <- paste(outbreaksGramma[outbreaksGramma != ""], collapse = "; ")
      
      ## column genLinkedNonContactPersonCaseIDs genLinkedNonContactPersonCaseIDCount
      #genLinkedNonContactPersonCaseIDs <- eva$`Linked to other sequenced Düsseldorf cases not part of other outbreaks and not listed as contacts (in either structured data or free-text entry fields of case records) of cases with direct genetic support (count)`[outbrakeID]
      genLinkedNonContactPersonCaseIDs <-  getCaseID(eva$"(Case IDs of previous column)...58"[outbrakeID])
      genLinkedNonContactPersonCaseIDs <- unique(genLinkedNonContactPersonCaseIDs)
      
      ##Check genetic distance and no contact person
      oubtreakKP <- edge2[edge2$CaseID_Index %in% cur_outbreack$Aktenzeichen & 
                             edge2$Typ == "SurvnetKontaktperson",]
      oubtreakKP <- oubtreakKP[oubtreakKP$indexSearch %in% edgesGraph$indexSearch,]
      
      
      genLinkedNonContactPersonCaseIDs <- genLinkedNonContactPersonCaseIDs[!genLinkedNonContactPersonCaseIDs %in% oubtreakKP$CaseID_Kontaktperson]
      genLinkedNonContactPersonCaseIDs <- genLinkedNonContactPersonCaseIDs[!genLinkedNonContactPersonCaseIDs %in% cur_outbreack$Aktenzeichen]
      nonstructuredKP <- getCaseID(eva$"(Case IDs of previous column)...39"[outbrakeID])
      genLinkedNonContactPersonCaseIDs <- genLinkedNonContactPersonCaseIDs[!genLinkedNonContactPersonCaseIDs %in% nonstructuredKP]
      
      ## check if genetic distance <= 1
      outlist <- data.frame(name=id, clonal="NA", n_clonal=0)
      
      if(!(is.na(genLinkedNonContactPersonCaseIDs[1]))){
         if(sum(genLinkedNonContactPersonCaseIDs != "") > 0){
            
            cases <- nodes[nodes$CaseID %in% genLinkedNonContactPersonCaseIDs,]
            cases <- cases[cases$SeqID != "",]
            
            conCases <- list()
            
            for(everyCase in  seq_len(nrow(cases))){
               
               #everyCase <- 1
               curCase <- cases[everyCase,]
               
               ##Get best case sequence
               c1 <- curCase$SeqID
               allCases_subset <- nodes2[nodes2$CaseID %in% "",]
               if(exists("connected")) allCases_subset <- nodes2[nodes2$CaseID %in% connected,]
               
               if(nrow(allCases_subset) > 0){
                  
                  ##Get best case sequence
                  c1All <- allCases_subset$SeqID
                  
                  cc <- expand.grid(c1, c1All)
                  cc2 <- expand.grid(curCase$CaseID, allCases_subset$CaseID)
                  
                  cc$case1 <- as.character(cc2$Var1)
                  cc$case2 <- as.character(cc2$Var2)
                  
                  cc$Var1 <- as.character(cc$Var1)
                  cc$Var2 <- as.character(cc$Var2)
                  
                  cc <- cc[cc$Var1 != cc$Var2,]
                  
                  cc$var1_inDM <- 0
                  cc$var2_inDM <- 0
                  
                  cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
                  cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
                  
                  cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
                  
                  if(nrow(cc) != 0){
                     
                     cc$dist <- lapply(1:nrow(cc), function(x){
                        
                        getGeneticDistance(cc$Var1[x], cc$Var2[x])
                        
                     })
                     
                     if(any(cc$dist <= 1)) conCases[[curCase$CaseID]] <-   curCase$CaseID
                     
                     
                  }
               }
            }
            
            
            outlist$clonal[1] <- paste(unique(unlist(conCases)), collapse=",")
            outlist$n_clonal[1] <- length(unique(unlist(conCases)))
         }
      }
      genLinkedNonContactPersonCaseIDs <- strsplit(outlist$clonal,",")[[1]]
      
      
      out$genLinkedNonContactPersonCaseIDs[outbrakeID]  <- paste(genLinkedNonContactPersonCaseIDs, collapse = ",")
      out$genLinkedNonContactPersonCaseIDCount[outbrakeID]  <- length(genLinkedNonContactPersonCaseIDs)
      
      if(genLinkedNonContactPersonCaseIDs[1] == "NA" | is.na(genLinkedNonContactPersonCaseIDs[1])){
         
         out$genLinkedNonContactPersonCaseIDs[outbrakeID]  <- ""
         out$genLinkedNonContactPersonCaseIDCount[outbrakeID]  <- 0
         
      } 
      
      
      ## column genLinkedNonContactPersonCaseIDMissed genLinkedNonContactPersonCaseIDMissedCount
      genLinkedNonContactPersonCaseIDsDMissed <- getCaseID(eva$"(Case IDs of previous column)...60"[outbrakeID])
      genLinkedNonContactPersonCaseIDsDMissed <- unique(genLinkedNonContactPersonCaseIDsDMissed)
      
      genLinkedNonContactPersonCaseIDsDMissed <- genLinkedNonContactPersonCaseIDsDMissed[genLinkedNonContactPersonCaseIDsDMissed %in% genLinkedNonContactPersonCaseIDs]
      
      ## check if genetic distance <= 1
      outlist <- data.frame(name=id, clonal="NA", n_clonal=0)
      
      if(!is.na(genLinkedNonContactPersonCaseIDsDMissed[1])){
         if(sum(genLinkedNonContactPersonCaseIDsDMissed != "") > 0){
            
            cases <- nodes[nodes$CaseID %in% genLinkedNonContactPersonCaseIDsDMissed,]
            cases <- cases[cases$SeqID != "",]
            
            conCases <- list()
            
            for(everyCase in  seq_len(nrow(cases))){
               
               #everyCase <- 1
               curCase <- cases[everyCase,]
               
               ##Get best case sequence
               c1 <- curCase$SeqID
               allCases_subset <- nodes2[nodes2$CaseID %in% "",]
               if(exists("connected")) allCases_subset <- nodes2[nodes2$CaseID %in% connected,]
               
               if(nrow(allCases_subset) > 0){
                  
                  ##Get best case sequence
                  c1All <- allCases_subset$SeqID
                  
                  cc <- expand.grid(c1, c1All)
                  cc2 <- expand.grid(curCase$CaseID, allCases_subset$CaseID)
                  
                  cc$case1 <- as.character(cc2$Var1)
                  cc$case2 <- as.character(cc2$Var2)
                  
                  cc$Var1 <- as.character(cc$Var1)
                  cc$Var2 <- as.character(cc$Var2)
                  
                  cc <- cc[cc$Var1 != cc$Var2,]
                  
                  cc$var1_inDM <- 0
                  cc$var2_inDM <- 0
                  
                  cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
                  cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
                  
                  cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
                  
                  if(nrow(cc) != 0){
                     
                     cc$dist <- lapply(1:nrow(cc), function(x){
                        
                        getGeneticDistance(cc$Var1[x], cc$Var2[x])
                        
                     })
                     
                     if(any(cc$dist <= 1)) conCases[[curCase$CaseID]] <-   curCase$CaseID
                     
                     
                  }
               }
            }
            
            
            outlist$clonal[1] <- paste(unique(unlist(conCases)), collapse=",")
            outlist$n_clonal[1] <- length(unique(unlist(conCases)))
         }
      }
      genLinkedNonContactPersonCaseIDsDMissed <- strsplit(outlist$clonal,",")[[1]]
      
      out$genLinkedNonContactPersonCaseIDMissed[outbrakeID]  <- paste(genLinkedNonContactPersonCaseIDsDMissed, collapse = ",")
      out$genLinkedNonContactPersonCaseIDMissedCount[outbrakeID]  <-length(genLinkedNonContactPersonCaseIDsDMissed)
      
      
      if(genLinkedNonContactPersonCaseIDsDMissed[1] == "NA" | is.na(genLinkedNonContactPersonCaseIDsDMissed[1])){
         
         out$genLinkedNonContactPersonCaseIDMissed[outbrakeID]  <- ""
         out$genLinkedNonContactPersonCaseIDMissedCount[outbrakeID]  <- 0
         
      } 
      
      
      
      ## column linkedNonContactPersonNonSequencedNonOutbreakCaseIDs linkedNonContactPersonNonSequencedNonOutbreakCaseIDsCount
      #linkedNonContactPersonNonSequencedNonOutbreakCaseIDs <- getCaseID(eva$`Linked to other non-sequenced Düsseldorf cases not part of other outbreaks and not listed as contacts (in either structured data or free-text entry fields of case records) of cases with suggestive genetic support (count)`[outbrakeID])
      linkedNonContactPersonNonSequencedNonOutbreakCaseIDs <- getCaseID(eva$`(Case IDs of previous column)...62`[outbrakeID])
      
      outbreakKP <- edge2$CaseID_Kontaktperson[edge2$CaseID_Index %in% cur_outbreack$Aktenzeichen & edge2$Typ == "SurvnetKontaktperson"]
      oubtreakKP <- oubtreakKP[oubtreakKP$indexSearch %in% edgesGraph$indexSearch,]
      nonstructuredKP <- getCaseID(eva$"(Case IDs of previous column)...39"[outbrakeID])
      linkedNonContactPersonNonSequencedNonOutbreakCaseIDs <- linkedNonContactPersonNonSequencedNonOutbreakCaseIDs[!linkedNonContactPersonNonSequencedNonOutbreakCaseIDs %in% outbreakKP]
      linkedNonContactPersonNonSequencedNonOutbreakCaseIDs <- linkedNonContactPersonNonSequencedNonOutbreakCaseIDs[!linkedNonContactPersonNonSequencedNonOutbreakCaseIDs %in% nonstructuredKP]
      
      out$linkedNonContactPersonNonSequencedNonOutbreakCaseIDs[outbrakeID]  <- paste(linkedNonContactPersonNonSequencedNonOutbreakCaseIDs, collapse = ",")
      out$linkedNonContactPersonNonSequencedNonOutbreakCaseIDsCount[outbrakeID]  <-length(linkedNonContactPersonNonSequencedNonOutbreakCaseIDs)
      
      if(linkedNonContactPersonNonSequencedNonOutbreakCaseIDs[1] == "NA" | is.na(linkedNonContactPersonNonSequencedNonOutbreakCaseIDs[1])){
         
         out$linkedNonContactPersonNonSequencedNonOutbreakCaseIDs[outbrakeID]  <- ""
         out$linkedNonContactPersonNonSequencedNonOutbreakCaseIDsCount[outbrakeID]  <- 0
         
      } 
      
      
      ##otherwiseLinkedCaseIDs
      otherwiseLinkedCaseIDs1 <- getCaseID(eva$`(Case IDs of previous column)...68`[outbrakeID])
      otherwiseLinkedCaseIDs2 <- getCaseID(eva$`(Case IDs of previous column)...66`[outbrakeID])
      otherwiseLinkedCaseIDs <- c(otherwiseLinkedCaseIDs1, otherwiseLinkedCaseIDs2)
      
      otherwiseLinkedCaseIDs <- otherwiseLinkedCaseIDs[!otherwiseLinkedCaseIDs %in% outbreakKP]
      otherwiseLinkedCaseIDs <- otherwiseLinkedCaseIDs[!otherwiseLinkedCaseIDs %in% nonstructuredKP]
      
      out$otherwiseLinkedCaseIDs[outbrakeID]  <- paste(otherwiseLinkedCaseIDs, collapse = ",")
      
      if(otherwiseLinkedCaseIDs[1] == "NA" | is.na(otherwiseLinkedCaseIDs[1])){
         
         out$otherwiseLinkedCaseIDs[outbrakeID]  <- ""
         
      } 
      
      
      
      
      ## column genLinkedNonContactPersonCaseIDMissed genLinkedNonContactPersonCaseIDMissedCount
      #linkedNonContactPersonNonSequencedNonOutbreakMissed <- getCaseID(eva$`... of these, that should have been index cases of the original outbreak (count)...39`[outbrakeID])
      linkedNonContactPersonNonSequencedNonOutbreakMissed <- getCaseID(eva$`(Case IDs of previous column)...64`[outbrakeID])
      
      linkedNonContactPersonNonSequencedNonOutbreakMissed <- linkedNonContactPersonNonSequencedNonOutbreakMissed[linkedNonContactPersonNonSequencedNonOutbreakMissed %in% linkedNonContactPersonNonSequencedNonOutbreakCaseIDs]
      
      out$linkedNonContactPersonNonSequencedNonOutbreakMissed[outbrakeID]  <- paste(linkedNonContactPersonNonSequencedNonOutbreakMissed, collapse = ",")
      out$linkedNonContactPersonNonSequencedNonOutbreakMissedCount[outbrakeID]  <-length(linkedNonContactPersonNonSequencedNonOutbreakMissed[linkedNonContactPersonNonSequencedNonOutbreakMissed != "0"])
      
      
      
      if(linkedNonContactPersonNonSequencedNonOutbreakMissed[1] == "NA" | is.na(linkedNonContactPersonNonSequencedNonOutbreakMissed[1])){
         
         out$linkedNonContactPersonNonSequencedNonOutbreakMissed[outbrakeID]  <-  ""
         out$linkedNonContactPersonNonSequencedNonOutbreakMissedCount[outbrakeID]  <- 0
         
      } 
      
      
      posContacts <- edge2[(edge2$CaseID_Index %in% cur_outbreack$Aktenzeichen & edge2$Typ == "SelbeAdresse")|
                              edge2$CaseID_Kontaktperson %in% cur_outbreack$Aktenzeichen & edge2$Typ == "SelbeAdresse",]
      posContacts <- posContacts[posContacts$indexSearch %in% edgesGraph$indexSearch,]
      
      ##find registered same address cases
      posContacts$reg[posContacts$CaseID_Index %in% cur_outbreack$Aktenzeichen &
                         posContacts$CaseID_Kontaktperson %in% cur_outbreack$Aktenzeichen] <- 1
      
      ## From same address cases, that were not registered for the outbreak, how many were sequenced
      # posContacts$otherCase <- ""
      posContacts$otherCase[posContacts$CaseID_Index %in% cur_outbreack$Aktenzeichen] <- posContacts$CaseID_Kontaktperson[posContacts$CaseID_Index %in% cur_outbreack$Aktenzeichen]
      posContacts$otherCase[posContacts$CaseID_Kontaktperson %in% cur_outbreack$Aktenzeichen] <- posContacts$CaseID_Index[posContacts$CaseID_Kontaktperson %in% cur_outbreack$Aktenzeichen]
      
      posContacts_seq <- posContacts[posContacts$reg == 0,]
      posContacts_seq <- posContacts_seq$otherCase
      
      linkedNonContactPersonNonSequencedNonOutbreakCaseIDsSameAdress <- linkedNonContactPersonNonSequencedNonOutbreakCaseIDs[linkedNonContactPersonNonSequencedNonOutbreakCaseIDs %in% posContacts$otherCase]
      linkedNonContactPersonNonSequencedNonOutbreakMissedCaseIDsSameAdress <- linkedNonContactPersonNonSequencedNonOutbreakCaseIDs[linkedNonContactPersonNonSequencedNonOutbreakCaseIDs %in% posContacts_seq]
      
      linkedNonContactPersonNonSequencedNonOutbreakCaseIDsCountSameAdressLinked <- linkedNonContactPersonNonSequencedNonOutbreakCaseIDsSameAdress
      linkedNonContactPersonNonSequencedNonOutbreakMissedCountSameAdressLinked <- linkedNonContactPersonNonSequencedNonOutbreakMissedCaseIDsSameAdress 
      
      if(linkedNonContactPersonNonSequencedNonOutbreakCaseIDs[1] == "NA" | is.na(linkedNonContactPersonNonSequencedNonOutbreakCaseIDs[1])){
         
         out$linkedNonContactPersonNonSequencedNonOutbreakCaseIDsCountSameAdressLinked[outbrakeID]  <- ""
         out$linkedNonContactPersonNonSequencedNonOutbreakMissedCountSameAdressLinked[outbrakeID]  <- 0
         
      } 
      
      ## Find purely genetically connected outbreaks
      linkedOutbreaks <- unique(outbreaks$AusbruchInfo_NameGA)
      
      conected_outbreaks <- list()
      
      for(conOut in linkedOutbreaks){
         
         conOut <- gsub(" ", "", conOut)
         
         if(conOut == id) next()
         conOutCases <- nodes2[nodes2$CaseID %in% outbreaks$Aktenzeichen[outbreaks$AusbruchInfo_NameGA == conOut ],]
         
         ##Calculate distance between both outbreak cases
         outlist <- data.frame(name=id, clonal="NA", n_clonal=0)
         
         posContacts_seq <- nodes2[nodes2$CaseID %in% "",]
         if(exists("connected")) posContacts_seq <- nodes2[nodes2$CaseID %in% connected,]
         
         
         if(nrow(posContacts_seq > 0)){
            
            cases <- posContacts_seq
            conCases <- list()
            
            for(everyCase in  seq_len(nrow(cases))){
               
               #everyCase <- 1
               curCase <- cases[everyCase,]
               
               ##Get best case sequence
               c1 <- curCase$SeqID
               allCases_subset <- conOutCases
               
               if(nrow(allCases_subset) > 0){
                  
                  ##Get best case sequence
                  c1All <- allCases_subset$SeqID
                  
                  cc <- expand.grid(c1, c1All)
                  cc2 <- expand.grid(curCase$CaseID, allCases_subset$CaseID)
                  
                  cc$case1 <- as.character(cc2$Var1)
                  cc$case2 <- as.character(cc2$Var2)
                  
                  cc$Var1 <- as.character(cc$Var1)
                  cc$Var2 <- as.character(cc$Var2)
                  
                  cc <- cc[cc$Var1 != cc$Var2,]
                  
                  cc$var1_inDM <- 0
                  cc$var2_inDM <- 0
                  
                  cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
                  cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
                  
                  cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
                  
                  if(nrow(cc) != 0){
                     
                     cc$dist <- lapply(1:nrow(cc), function(x){
                        
                        getGeneticDistance(cc$Var1[x], cc$Var2[x])
                        
                     })
                     
                     if(any(cc$dist <= 1)) conCases[[curCase$CaseID]] <-   curCase$CaseID
                     
                     
                  }
               }
            }
            
            
            outlist$clonal[1] <- paste(unique(unlist(conCases)), collapse=",")
            outlist$n_clonal[1] <- length(unique(unlist(conCases)))
            if(outlist$n_clonal > 0) conected_outbreaks[[conOut]] <- conOut
         }
         
         
         
         
      }
      
      out$genLinkedOutbreakIDs[outbrakeID] <- paste(unlist(conected_outbreaks), collapse = ", ")
      
      
      ## casesLinkedByEpidemiologicalDataCaseIDs
      out$casesLinkedByEpidemiologicalDataCaseIDs[outbrakeID] <- paste(unique(getCaseID(eva$"(Case IDs of previous column)...66"[outbrakeID])), collapse = ", ")
      
      ## erroneouslyAssignedCasesCaseIDs
      out$erroneouslyAssignedCasesCaseIDs[outbrakeID] <- paste(unique(getCaseID(eva$"(Case IDs for the previous column)...16"[outbrakeID])), collapse = ", ")
      
      
      
      ##Cleanup
      objectNames <- ls()
      # objectNames <- objectNames[!objectNames %in% c("eva", "clust", "dm", "edge", "edge2", "nodes", 
      #                                                "nodes2", "outbreaks", "samples_ids",
      #                                                "getGeneticDistance", "SeqID_Ns", "out",
      #                                                "outbrakeID", "getCaseID", "case_matching",
      #                                                "edgesGraph","connected","c2", "cc","cc2",
      #                                                "cc4", "cc5", "conCases", "conected_outbreaks",
      #                                                "connected_cluster","conOutCases",
      #                                                "posContacts", "posContacts_seq",
      #                                                "connected2", "connectedIDsOutbreak", "outbreakKP",
      #                                                "outbreak")]
      
      objectNames <- objectNames[!objectNames %in% c("eva", "clust", "dm", "edge", "edge2", "nodes",
                                                     "nodes2", "outbreaks", "samples_ids",
                                                     "getGeneticDistance", "SeqID_Ns", "out",
                                                     "outbrakeID","outbrakeIDs", "getCaseID", "case_matching",
                                                     "edgesGraph","connected",
                                                     "outbreak","outbreaks")]
      
      rm(list =objectNames)
      
   }else{
      
      out$month[outbrakeID] <- "outbreak not found"
      rm(id, cur_outbreack)
   }
}
options(warn=1)


## Correct missassigend cases for nonclonal outbreaks with 2 cases
out$misassignedCases[out$n_sequencedCases == 2 & out$sequencedCases_clonal == "not clonal"] <- 1
out$outbreakType <- outbreaks$type3[match(out$outbreakID, outbreaks$AusbruchInfo_NameGA )]

 
# write.csv2(out, file = paste(outbreakTableNamePath,".validated.csv"), row.names=F)
# 
save(out, file="mock_out")


out$Duration_days <-  out$Duration_days +1

# write.csv2(out, file = paste("Outbreak summary table validated.csv"), row.names=F)


sum(out$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinked)
sum(out$n_positiveSameAddressCasesNotRegisteredSequencedGenLinked)
sum(out$n_positiveSameAddressCasesNotRegisteredSequencedGenLinkedNotContact)

load("mock_out")


## Create outbreak summary table

os <- data.frame(
   category=unique(out$outbreakType),
   n_outbreaks=0,
   n_cases=0,
   
   # sequencing informed QC
   n_outbreaksDistanceAvailable =0,                 #  Number of outbreaks with ≥ 2 sequenced cases	
   n_outbreaksNonClonal =0,                         #  Number of non-clonal outbreaks	
   n_sequencedCases = 0,                            #  Number of sequenced cases
   n_sequencedCasesInoutbreaksDistanceAvailable=0,  #  Number of sequenced cases in outbreaks with ≥2 sequenced cases	
   n_erroneousCases=0,                              #  Erroneously assigned cases
   
   # Linked community cases, routine health authority data
   # ("Forward contact tracing" ∪ "backward contact tracing" ∪ "sameAddress")
   n_linkedCasesTotal=0,
   n_linkedCasesSeqBasedAssesed=0,
   n_linkedCasesSeqBasedAssesedAccepted=0,
   
   n_totalForwardContactTracingRecords=0,
   
   # Forward contact tracing
   n_forwardContactTracingTotal=0,
   n_forwardContactTracingSeqBasedAssesed=0,
   n_forwardContactTracingSeqBasedAssesedAccepted=0,
   
   # Backward contact tracing
   n_backwardContactTracingTotal=0,
   n_backwardContactTracingCommunityInfectedByOutbreak=0,
   n_backwardContactTracingOutbreakInfectedByCommunity=0,
   n_backwardContactTracingSeqBasedAssesed=0,
   n_backwardContactTracingSeqBasedAssesedAccepted=0,
   
   # Same Address
   n_sameAddressTotal=0,
   n_sameAddressSeqBasedAssesed=0,
   n_sameAddressSeqBasedAssesedAccepted=0,
   
   identifiedOutbreakLinks="",
   
   ## Genetically identical community cases
   n_genIdenticalCommunityCases=0,
   n_genIdenticalCommunityCasesNotConnectedViaEpiData=0,
   
   ## Nonstructured Linked cases
   n_nonstructuredLinkedCases=0,
   n_nonstructuredLinkedCasesSeqBasedAssesed=0,
   n_nonstructuredLinkedCasesSeqBasedAssesedAccepted=0,
   
   ## Additionally linked cases manual analysis
   n_additionalLinkedSeqConfirmed=0,
   n_additionalLinkedIndirectSeqBasedEpiLinked=0,
   n_additionalOtherwiseLinked=0,
   
   
   ## Missing outbreak case
   n_totalMissingCases=0,
   n_missedGenIdenticalCommunityCases=0,
   n_missedGenIdenticalCommunityCasesPercentage=0,
   n_missedForwardContactTracingSeqBasedAssesedAccepted=0,
   n_missedForwardContactTracingSeqBasedAssesedAcceptedPercentage=0,
   n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted=0,
   n_missedNonstructuredLinkedCasesSeqBasedAssesedAcceptedPercentage=0,
   n_missedAdditionalLinkedSeqConfirmed=0,
   n_missedAdditionalLinkedSeqConfirmedPercentag=0,
   
   n_missedAdditionalLinkedIndirectSeqBasedEpiLinked=0,
   n_missedAdditionalOtherwiseLinked=0,
   
   ##  Linked community cases after manual review with genetic support	
   n_linkedCommunityCasesManualReviewSeqBasedAssesedAccepted=0,
   n_linkedCommunityCasesManualReviewSeqBasedAssesedAcceptedPlusIndirectGeneticEvidence=0,
   
   n_erroneouslyAssignedCasesManualReviewSequenced=0,
   n_erroneouslyAssignedCasesManualReviewNotSequenced=0,
   
   externalGeneticLinksInvestigated= "",
   non_structuredLinksInvestigated= ""
)


collapseCol <- function(vec){
   vec <- vec[!is.na(vec)]
   vec <- vec[vec != "NA"]
   paste(vec, collapse= ",")
}


getCaseID <- function(column){
   
   test <- column[column != "NA"]
   test <- na.omit(test)
   
   if(length(test) > 0 & any(test != "")){
      test <-  strsplit(test, "or")
      test <- lapply(test, function(x) {strsplit(x, ",")})
      test <- unlist(test)
      test <- strsplit(test, "\\n")
      test <- unlist(test)
      test <- strsplit(test, " ")
      test <- unlist(test)
      test <- strsplit(test, ":")
      test <- unlist(test)
      test <- strsplit(test, "'")
      test <- unlist(test)
      test <- gsub("\\(", "", test)
      test <- gsub("\\)", "", test)
      test <- gsub(" ", "", test)
      t2 <- data.frame(name=test)
      t2$start <- substr(t2$name, 1, 3)
      t2 <- t2[t2$start == "CVD",]
      t2$name <- gsub("\r", "",  t2$name)
      
      return(t2$name)
   }else{
      
      return(column[column != ""])
   }
}


length2 <- function(inputVector){
   
   inputVector2 <- inputVector[inputVector != "NA"]
   length(na.omit(inputVector2))
}


for(i in 1:nrow(os)){
   
   #i <- 1
   outbreakType <- os$category[i]
   
   subOut <- out[out$outbreakType == outbreakType,]
   
   os$n_outbreaks[i] <- nrow(subOut)
   os$n_cases[i] <- sum(subOut$n_casesDuesseldorf)
   
   # sequencing informed QC
   os$n_outbreaksDistanceAvailable[i] <- nrow(subOut[subOut$n_sequencedCases>=2,])
   os$n_outbreaksNonClonal[i] <- nrow(subOut[subOut$sequencedCases_clonal == "not clonal",])
   os$n_sequencedCasesInoutbreaksDistanceAvailable[i] <- sum(subOut$n_sequencedCases[subOut$n_sequencedCases>=2])
   os$n_sequencedCases[i] <-  sum(subOut$n_sequencedCases)
   os$n_erroneousCases[i] <- sum(as.numeric(subOut$misassignedCases[subOut$sequencedCases_clonal == "not clonal"]))
   
   
   os$n_totalForwardContactTracingRecords[i] <-  sum(subOut$n_contactPersonsDuesseldorf)
   
   
   # Forward contact tracing
   subCases <- outbreaks$Aktenzeichen[outbreaks$AusbruchInfo_NameGA %in% subOut$outbreakID]
   rmCaseIDs <- function(CaseIDVector) {
      CaseIDVector[!CaseIDVector %in% subCases]
   }
   n_forwardContactTracingTotal <- unique(getCaseID(subOut$n_positiveContactPersonsDuesseldorfNotRegisteredCaseIDs))
   n_forwardContactTracingTotal <- rmCaseIDs(n_forwardContactTracingTotal)
   os$n_forwardContactTracingTotal[i] <- length2(n_forwardContactTracingTotal)
   
   ac <- unique(getCaseID(subOut$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs))
   rej<- unique(getCaseID(subOut$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenNOTLinkedCaseID))
   ac <- rmCaseIDs(ac)
   rej <- rmCaseIDs(rej)
   n_forwardContactTracingSeqBasedAssesed <- c(ac, rej)
   n_forwardContactTracingSeqBasedAssesedAccepted <- ac
   os$n_forwardContactTracingSeqBasedAssesed[i] <- length2(ac) + length2(rej)
   os$n_forwardContactTracingSeqBasedAssesedAccepted[i] <- length2(ac)
   
   
   
   # Backward contact tracing
   n_backwardContactTracingCommunityInfectedByOutbreak <- unique(getCaseID(subOut$n_communityCasesInfectedByOutbreakCasesNotRegisteredCaseIDs))
   n_backwardContactTracingOutbreakInfectedByCommunity <- unique(getCaseID(subOut$n_OutbreakCasesInfectedBycommunityCasesNotRegisteredCaseIDs))
   n_backwardContactTracingCommunityInfectedByOutbreak <- rmCaseIDs(n_backwardContactTracingCommunityInfectedByOutbreak)
   n_backwardContactTracingOutbreakInfectedByCommunity <- rmCaseIDs(n_backwardContactTracingOutbreakInfectedByCommunity)
   
   os$n_backwardContactTracingCommunityInfectedByOutbreak[i] <- length2(n_backwardContactTracingCommunityInfectedByOutbreak)
   os$n_backwardContactTracingOutbreakInfectedByCommunity[i] <-  length2(n_backwardContactTracingOutbreakInfectedByCommunity)
   
   os$n_backwardContactTracingTotal[i] <- length2(n_backwardContactTracingCommunityInfectedByOutbreak) + length2(n_backwardContactTracingOutbreakInfectedByCommunity) 
   ac <- unique(getCaseID(subOut$n_NotRegisteredInfectedByLinksTotalSequencedGenLinkedCaseIDs))
   rej<- unique(getCaseID(subOut$n_NotRegisteredInfectedByLinksTotalSequencedGenNotLinkedCaseIDs))
   ac <- rmCaseIDs(ac)
   rej <- rmCaseIDs(rej)
   n_backwardContactTracingSeqBasedAssesed <- c(ac, rej)
   n_backwardContactTracingSeqBasedAssesedAccepted <- ac
   os$n_backwardContactTracingSeqBasedAssesed[i] <- length2(ac) + length2(rej)
   os$n_backwardContactTracingSeqBasedAssesedAccepted[i] <- length2(ac)
   
   
   
   # Same Address
   n_sameAddressTotal  <- unique(getCaseID(subOut$n_positiveSameAddressCasesNotRegisteredCaseIDs))
   os$n_sameAddressTotal[i] <- length2(rmCaseIDs(n_sameAddressTotal))
   ac <- unique(getCaseID(subOut$n_positiveSameAddressCasesNotRegisteredSequencedGenLinkedCaseIDs))
   rej<- unique(getCaseID(subOut$n_positiveSameAddressCasesNotRegisteredSequencedGenNOTLinkedCaseIDs))
   ac <- rmCaseIDs(ac)
   rej <- rmCaseIDs(rej)
   n_sameAddressSeqBasedAssesed <- c(ac, rej)
   n_sameAddressSeqBasedAssesedAccepted <- ac
   os$n_sameAddressSeqBasedAssesed[i] <- length2(ac) + length2(rej)
   os$n_sameAddressSeqBasedAssesedAccepted[i] <- length2(ac)
   
   
   
   # Linked community cases, routine health authority data
   # ("Forward contact tracing" ∪ "backward contact tracing" ∪ "sameAddress")
   # Summarize now MAKE UNIQUE
   lct <- unique(rmCaseIDs(c(n_sameAddressTotal, n_backwardContactTracingCommunityInfectedByOutbreak, 
                             n_backwardContactTracingOutbreakInfectedByCommunity, n_forwardContactTracingTotal)))
   os$n_linkedCasesTotal[i] <- length2(lct)
   
   lctsa <- unique(rmCaseIDs(c(n_sameAddressSeqBasedAssesed, n_backwardContactTracingSeqBasedAssesed,
                               n_forwardContactTracingSeqBasedAssesed)))
   os$n_linkedCasesSeqBasedAssesed[i] <- length2(lctsa)
   
   lctsaa <- unique(rmCaseIDs(c(n_sameAddressSeqBasedAssesedAccepted, n_backwardContactTracingSeqBasedAssesedAccepted,
                                n_forwardContactTracingSeqBasedAssesedAccepted)))
   os$n_linkedCasesSeqBasedAssesedAccepted[i] <- length2(lctsaa)
   
   ## Genetically identical community cases
   n_genIdenticalCommunityCases <- unique(getCaseID(subOut$genLinkedCommunityCasesCaseIDs))
   os$n_genIdenticalCommunityCases[i] <- length(rmCaseIDs(n_genIdenticalCommunityCases))
   n_genIdenticalCommunityCasesNotConnectedViaEpiData <- unique(getCaseID(subOut$genLinkedCommunityCasesnoEdgeCaseIDs))
   os$n_genIdenticalCommunityCasesNotConnectedViaEpiData[i] <- length(rmCaseIDs(n_genIdenticalCommunityCasesNotConnectedViaEpiData))
   
   
   
   ## Nonstructured Linked cases
   healtCareLinkIDs <- c(n_sameAddressTotal, n_backwardContactTracingCommunityInfectedByOutbreak,
                         n_backwardContactTracingOutbreakInfectedByCommunity, n_forwardContactTracingTotal  )
   
   rmCaseIDs2 <-  function(CaseIDVector, target) {
      CaseIDVector[!CaseIDVector %in% target]
   }
   
   n_nonstructuredLinkedCases <- unique(getCaseID(subOut$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredCaseIDs))
   os$n_nonstructuredLinkedCases[i] <- length2(rmCaseIDs2(n_nonstructuredLinkedCases,healtCareLinkIDs))
   ac <- unique(getCaseID(subOut$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs))
   rej<- unique(getCaseID(subOut$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenNOTLinkedCaseIDs))
   ac <- rmCaseIDs2(ac,healtCareLinkIDs)
   rej <- rmCaseIDs2(rej,healtCareLinkIDs)
   os$n_nonstructuredLinkedCasesSeqBasedAssesed[i] <- length2(ac) + length2(rej)
   os$n_nonstructuredLinkedCasesSeqBasedAssesedAccepted[i] <- length2(ac)
   
   
   
   
   
   ## Additionally linked cases manual analysis
   healtCareLinkIDs <- c(n_sameAddressTotal, n_backwardContactTracingCommunityInfectedByOutbreak,
                         n_backwardContactTracingOutbreakInfectedByCommunity, n_forwardContactTracingTotal,
                         n_nonstructuredLinkedCases
   )
   
   n_additionalLinkedSeqConfirmed <- unique(getCaseID(subOut$genLinkedNonContactPersonCaseIDs))
   n_additionalLinkedSeqConfirmed <- rmCaseIDs2(n_additionalLinkedSeqConfirmed,healtCareLinkIDs)
   os$n_additionalLinkedSeqConfirmed[i] <-  length2(n_additionalLinkedSeqConfirmed)
   
   
   n_additionalLinkedIndirectSeqBasedEpiLinked <- unique(getCaseID(subOut$linkedNonContactPersonNonSequencedNonOutbreakCaseIDs))
   os$n_additionalLinkedIndirectSeqBasedEpiLinked[i] <- length2(rmCaseIDs2(n_additionalLinkedIndirectSeqBasedEpiLinked,healtCareLinkIDs))
   
   n_additionalOtherwiseLinked <- unique(getCaseID(subOut$otherwiseLinkedCaseIDs))
   n_additionalOtherwiseLinked <-  rmCaseIDs2(n_additionalOtherwiseLinked,healtCareLinkIDs)
   os$n_additionalOtherwiseLinked[i] <- length2(n_additionalOtherwiseLinked)
   
   
   
   
   
   
   ## Missing outbreak case
   n_missedForwardContactTracingSeqBasedAssesedAccepted <- unique(getCaseID(subOut$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissedCaseID))
   n_missedForwardContactTracingSeqBasedAssesedAccepted <- rmCaseIDs(n_missedForwardContactTracingSeqBasedAssesedAccepted)
   os$n_missedForwardContactTracingSeqBasedAssesedAccepted[i] <- length2(n_missedForwardContactTracingSeqBasedAssesedAccepted)
   pc <-  round(os$n_missedForwardContactTracingSeqBasedAssesedAccepted[i] /    os$n_forwardContactTracingSeqBasedAssesedAccepted[i]*100, 1)
   os$n_missedForwardContactTracingSeqBasedAssesedAcceptedPercentage[i] <- pc
   
   
   n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted <- unique(getCaseID(subOut$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissedCaseID))
   n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted <- rmCaseIDs2(n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted,healtCareLinkIDs)
   os$n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted[i] <- length2(n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted)
   pc <-  round(os$n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted[i] /    os$n_nonstructuredLinkedCasesSeqBasedAssesedAccepted[i]*100, 1)
   os$n_missedNonstructuredLinkedCasesSeqBasedAssesedAcceptedPercentage[i] <- pc
   
   n_missedAdditionalLinkedSeqConfirmed <- unique(getCaseID(subOut$genLinkedNonContactPersonCaseIDMissed))
   n_missedAdditionalLinkedSeqConfirmed <- rmCaseIDs2(n_missedAdditionalLinkedSeqConfirmed,healtCareLinkIDs)
   os$n_missedAdditionalLinkedSeqConfirmed[i] <- length2(n_missedAdditionalLinkedSeqConfirmed)
   pc <-  round( os$n_missedAdditionalLinkedSeqConfirmed[i]  /    os$n_additionalLinkedSeqConfirmed[i]*100, 1)
   os$n_missedAdditionalLinkedSeqConfirmedPercentag[i] <- pc
   
   
   
   Hmissed <- c(n_missedForwardContactTracingSeqBasedAssesedAccepted,
                n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted,
                n_missedAdditionalLinkedSeqConfirmed)
   
   colH <- c(n_sameAddressTotal, n_backwardContactTracingCommunityInfectedByOutbreak,
             n_backwardContactTracingOutbreakInfectedByCommunity, n_forwardContactTracingTotal)
   
   os$n_totalMissingCases[i] <- length2(unique(Hmissed))
   
   n_missedGenIdenticalCommunityCases <- Hmissed[Hmissed %in% colH]
   os$n_missedGenIdenticalCommunityCases[i] <- length2(n_missedGenIdenticalCommunityCases)
   
   a1 <- unique(getCaseID(subOut$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs))
   a1 <- rmCaseIDs(a1)
   a2 <- unique(getCaseID(subOut$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs))
   a2 <- rmCaseIDs(a2)
   a3 <- unique(getCaseID(subOut$genLinkedNonContactPersonCaseIDs))
   a3 <- rmCaseIDs(a3)
   
   HNotmissed <- c(a1, a2, a3)
   
   os$n_missedGenIdenticalCommunityCasesPercentage[i] <- round(os$n_missedGenIdenticalCommunityCases[i] / length2(unique(HNotmissed))*100,1)
   
   
   healtCareLinkIDs2 <- c(n_sameAddressTotal, n_backwardContactTracingCommunityInfectedByOutbreak,
                          n_backwardContactTracingOutbreakInfectedByCommunity, n_forwardContactTracingTotal,
                          n_nonstructuredLinkedCases, n_additionalLinkedSeqConfirmed, n_additionalOtherwiseLinked,
                          n_missedGenIdenticalCommunityCases, n_missedForwardContactTracingSeqBasedAssesedAccepted,
                          n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted,
                          n_missedAdditionalLinkedSeqConfirmed  )
   
   n_missedAdditionalLinkedIndirectSeqBasedEpiLinked <- unique(getCaseID(subOut$linkedNonContactPersonNonSequencedNonOutbreakMissed))
   n_missedAdditionalLinkedIndirectSeqBasedEpiLinked <- rmCaseIDs2(n_missedAdditionalLinkedIndirectSeqBasedEpiLinked,healtCareLinkIDs2)
   os$n_missedAdditionalLinkedIndirectSeqBasedEpiLinked[i] <- length2(n_missedAdditionalLinkedIndirectSeqBasedEpiLinked)
   
   
   
   n_missedAdditionalOtherwiseLinked <- unique(getCaseID(subOut$casesLinkedByEpidemiologicalDataCaseIDs))
   n_missedAdditionalOtherwiseLinked <- rmCaseIDs2(n_missedAdditionalOtherwiseLinked,unique(Hmissed))
   os$n_missedAdditionalOtherwiseLinked[i] <- length2(n_missedAdditionalOtherwiseLinked)
   
   
   
   
   ##  Linked community cases after manual review with genetic support	
   # J, AB, AC minus C,  AF, AO, AP
   J <- c( unique(getCaseID(subOut$n_positiveSameAddressCasesNotRegisteredSequencedGenLinkedCaseIDs)),
           unique(getCaseID(subOut$n_NotRegisteredInfectedByLinksTotalSequencedGenLinkedCaseIDs)),
           unique(getCaseID(subOut$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs)))
   J <- rmCaseIDs(J)
   
   AB <- unique(getCaseID(subOut$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs))
   AB <- rmCaseIDs2(AB,healtCareLinkIDs)
   
   AC <- unique(getCaseID(subOut$genLinkedNonContactPersonCaseIDs))
   AC <- rmCaseIDs2(AC,healtCareLinkIDs)
   
   A1 <- unique(c(J, AB, AC))
   
   C  <- subCases
   AF <- Hmissed
   AO <-unique(getCaseID(subOut$linkedNonContactPersonNonSequencedNonOutbreakMissed))
   AO <- rmCaseIDs2(AO,healtCareLinkIDs2)
   AP <- n_missedAdditionalOtherwiseLinked
   
   A2 <- c(C, AF,AO,AP)
   
   os$n_linkedCommunityCasesManualReviewSeqBasedAssesedAccepted[i] <- sum(!A1 %in% A2)
   
   
   A3 <- unique(c(J, AB, AC, rmCaseIDs2(n_additionalLinkedIndirectSeqBasedEpiLinked,healtCareLinkIDs)))
   
   os$n_linkedCommunityCasesManualReviewSeqBasedAssesedAcceptedPlusIndirectGeneticEvidence[i] <-  sum(!A3 %in% A2)
   
   
   
   n_erroneouslyAssignedCasesManualReviewSequenced <- unique(getCaseID(subOut$erroneouslyAssignedCasesCaseIDs))
   n_erroneouslyAssignedCasesManualReviewSequenced <- n_erroneouslyAssignedCasesManualReviewSequenced[!n_erroneouslyAssignedCasesManualReviewSequenced %in% subOut$misassignedCasesCaseIDs]
   n_erroneouslyAssignedCasesManualReviewSequenced <- n_erroneouslyAssignedCasesManualReviewSequenced[!n_erroneouslyAssignedCasesManualReviewSequenced %in% subCases[!subCases %in% nodes2$CaseID]]
   if(length(n_erroneouslyAssignedCasesManualReviewSequenced) != 0) os$n_erroneouslyAssignedCasesManualReviewSequenced[i] <- length2(n_erroneouslyAssignedCasesManualReviewSequenced[n_erroneouslyAssignedCasesManualReviewSequenced != "NA"])
   
   n_erroneouslyAssignedCasesManualReviewNotSequenced <- unique(getCaseID(subOut$erroneouslyAssignedCasesCaseIDs))
   n_erroneouslyAssignedCasesManualReviewNotSequenced <- n_erroneouslyAssignedCasesManualReviewNotSequenced[!n_erroneouslyAssignedCasesManualReviewNotSequenced %in% subOut$misassignedCasesCaseIDs]
   n_erroneouslyAssignedCasesManualReviewNotSequenced <- n_erroneouslyAssignedCasesManualReviewNotSequenced[!n_erroneouslyAssignedCasesManualReviewNotSequenced %in% subCases[subCases %in% nodes2$CaseID]]
   if(length(n_erroneouslyAssignedCasesManualReviewNotSequenced) != 0)  os$n_erroneouslyAssignedCasesManualReviewNotSequenced[i] <- length2(n_erroneouslyAssignedCasesManualReviewNotSequenced[n_erroneouslyAssignedCasesManualReviewNotSequenced != "NA"])
   
   eGlI <- as.character(subOut$externalGenLinksInverstigated)
   eGlI <- lapply(eGlI, function(egli){
      gsub(" ", "", strsplit(egli, "\r")[[1]][[1]])
   })
   eGlI[eGlI == "NA"] <- 0
   eGlI <- as.numeric(as.character(eGlI))
   os$externalGeneticLinksInvestigated[i] <- sum(eGlI)
   os$non_structuredLinksInvestigated[i] <-  sum(subOut$nonstructuredContactsAnalysed)
}

write.csv2(os, file="mock_os.csv", row.names=F)


### Do the same with CaseIDs
osOutbreak <- data.frame(
   outbreakID=out$outbreakID,
   category="",
   n_outbreaks="",
   n_cases="",
   
   # sequencing informed QC
   n_outbreaksDistanceAvailable ="",                 #  Number of outbreaks with ≥ 2 sequenced cases	
   n_outbreaksNonClonal ="",                         #  Number of non-clonal outbreaks	
   n_sequencedCasesInoutbreaksDistanceAvailable="",  #  Number of sequenced cases in outbreaks with ≥2 sequenced cases	
   n_erroneousCases="",                              #  Erroneously assigned cases
   
   # Linked community cases, routine health authority data
   # ("Forward contact tracing" ∪ "backward contact tracing" ∪ "sameAddress")
   n_linkedCasesTotal="",
   n_linkedCasesSeqBasedAssesed="",
   n_linkedCasesSeqBasedAssesedAccepted="",
   
   n_totalForwardContactTracingRecords="",
   
   # Forward contact tracing
   n_forwardContactTracingTotal="",
   n_forwardContactTracingSeqBasedAssesed="",
   n_forwardContactTracingSeqBasedAssesedAccepted="",
   
   # Backward contact tracing
   n_backwardContactTracingTotal="",
   n_backwardContactTracingCommunityInfectedByOutbreak="",
   n_backwardContactTracingOutbreakInfectedByCommunity="",
   n_backwardContactTracingSeqBasedAssesed="",
   n_backwardContactTracingSeqBasedAssesedAccepted="",
   
   # Same Address
   n_sameAddressTotal="",
   n_sameAddressSeqBasedAssesed="",
   n_sameAddressSeqBasedAssesedAccepted="",
   
   identifiedOutbreakLinks="",
   
   ## Genetically identical community cases
   n_genIdenticalCommunityCases="",
   n_genIdenticalCommunityCasesNotConnectedViaEpiData="",
   
   ## Nonstructured Linked cases
   n_nonstructuredLinkedCases="",
   n_nonstructuredLinkedCasesSeqBasedAssesed="",
   n_nonstructuredLinkedCasesSeqBasedAssesedAccepted="",
   
   ## Additionally linked cases manual analysis
   n_additionalLinkedSeqConfirmed="",
   n_additionalLinkedIndirectSeqBasedEpiLinked="",
   n_additionalOtherwiseLinked="",
   
   
   ## Missing outbreak case
   n_totalMissingCases="",
   n_missedGenIdenticalCommunityCases="",
   n_missedGenIdenticalCommunityCasesPercentage="",
   n_missedForwardContactTracingSeqBasedAssesedAccepted="",
   n_missedForwardContactTracingSeqBasedAssesedAcceptedPercentage="",
   n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted="",
   n_missedNonstructuredLinkedCasesSeqBasedAssesedAcceptedPercentage="",
   n_missedAdditionalLinkedSeqConfirmed="",
   n_missedAdditionalLinkedSeqConfirmedPercentag="",
   
   n_missedAdditionalLinkedIndirectSeqBasedEpiLinked="",
   n_missedAdditionalOtherwiseLinked="",
   
   ##  Linked community cases after manual review with genetic support	
   n_linkedCommunityCasesManualReviewSeqBasedAssesedAccepted="",
   n_linkedCommunityCasesManualReviewSeqBasedAssesedAcceptedPlusIndirectGeneticEvidence="",
   
   n_erroneouslyAssignedCasesManualReviewSequenced="",
   n_erroneouslyAssignedCasesManualReviewNotSequenced="",
   externalGeneticLinksInvestigated = "",
   non_structuredLinksInvestigated = ""
)


for(i in 1:nrow(osOutbreak)){
   
   #i <- 46
   outbreakID <- osOutbreak$outbreakID[i]
   osOutbreak$category[i] <- out$outbreakType[out$outbreakID == outbreakID]
   
   subOut <- out[out$outbreakID == outbreakID,]
   subOutCases <- outbreaks[outbreaks$AusbruchInfo_NameGA == outbreakID,]
   
   osOutbreak$n_outbreaks[i] <- 1
   osOutbreak$n_cases[i] <- nrow(subOutCases)
   
   # sequencing informed QC
   osOutbreak$n_outbreaksDistanceAvailable[i] <- subOut$n_sequencedCases >=2
   osOutbreak$n_outbreaksNonClonal[i] <- subOut$sequencedCases_clonal 
   osOutbreak$n_sequencedCasesInoutbreaksDistanceAvailable[i] <- subOut$n_sequencedCases
   osOutbreak$n_erroneousCases[i] <- subOut$misassignedCasesCaseIDs
   
   
   osOutbreak$n_totalForwardContactTracingRecords[i] <-  subOut$n_contactPersonsDuesseldorf
   
   
   # Forward contact tracing
   subCases <- subOutCases$Aktenzeichen
   rmCaseIDs <- function(CaseIDVector) {
      CaseIDVector[!CaseIDVector %in% subCases]
   }
   
   n_forwardContactTracingTotal <- unique(getCaseID(subOut$n_positiveContactPersonsDuesseldorfNotRegisteredCaseIDs))
   n_forwardContactTracingTotal <- rmCaseIDs(n_forwardContactTracingTotal)
   osOutbreak$n_forwardContactTracingTotal[i] <- collapseCol(n_forwardContactTracingTotal)
   
   ac <- unique(getCaseID(subOut$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs))
   rej<- unique(getCaseID(subOut$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenNOTLinkedCaseID))
   ac <- rmCaseIDs(ac)
   rej <- rmCaseIDs(rej)
   all <- c(ac, rej)
   osOutbreak$n_forwardContactTracingSeqBasedAssesed[i] <- collapseCol(all)
   osOutbreak$n_forwardContactTracingSeqBasedAssesedAccepted[i] <- collapseCol(ac)
   
   
   
   # Backward contact tracing
   n_backwardContactTracingCommunityInfectedByOutbreak <- unique(getCaseID(subOut$n_communityCasesInfectedByOutbreakCasesNotRegisteredCaseIDs))
   n_backwardContactTracingOutbreakInfectedByCommunity <- unique(getCaseID(subOut$n_OutbreakCasesInfectedBycommunityCasesNotRegisteredCaseIDs))
   n_backwardContactTracingCommunityInfectedByOutbreak <- rmCaseIDs(n_backwardContactTracingCommunityInfectedByOutbreak)
   n_backwardContactTracingOutbreakInfectedByCommunity <- rmCaseIDs(n_backwardContactTracingOutbreakInfectedByCommunity)
   
   osOutbreak$n_backwardContactTracingCommunityInfectedByOutbreak[i] <- collapseCol(n_backwardContactTracingCommunityInfectedByOutbreak)
   osOutbreak$n_backwardContactTracingOutbreakInfectedByCommunity[i] <-  collapseCol(n_backwardContactTracingOutbreakInfectedByCommunity)
   
   osOutbreak$n_backwardContactTracingTotal[i] <- collapseCol(c(n_backwardContactTracingCommunityInfectedByOutbreak,n_backwardContactTracingOutbreakInfectedByCommunity) )
   ac <- unique(getCaseID(subOut$n_NotRegisteredInfectedByLinksTotalSequencedGenLinkedCaseIDs))
   rej<- unique(getCaseID(subOut$n_NotRegisteredInfectedByLinksTotalSequencedGenNotLinkedCaseIDs))
   ac <- rmCaseIDs(ac)
   rej <- rmCaseIDs(rej)
   all <- c(ac, rej)
   osOutbreak$n_backwardContactTracingSeqBasedAssesed[i] <-  collapseCol(all)
   osOutbreak$n_backwardContactTracingSeqBasedAssesedAccepted[i] <-collapseCol(ac)
   
   
   
   # Same Address
   n_sameAddressTotal  <- unique(getCaseID(subOut$n_positiveSameAddressCasesNotRegisteredCaseIDs))
   osOutbreak$n_sameAddressTotal[i] <- collapseCol(rmCaseIDs(n_sameAddressTotal))
   ac <- unique(getCaseID(subOut$n_positiveSameAddressCasesNotRegisteredSequencedGenLinkedCaseIDs))
   rej<- unique(getCaseID(subOut$n_positiveSameAddressCasesNotRegisteredSequencedGenNOTLinkedCaseIDs))
   ac <- rmCaseIDs(ac)
   rej <- rmCaseIDs(rej)
   all <- c(ac, rej)
   osOutbreak$n_sameAddressSeqBasedAssesed[i] <- collapseCol(all)
   osOutbreak$n_sameAddressSeqBasedAssesedAccepted[i] <-collapseCol(ac)
   
   
   # Linked community cases, routine health authority data
   # ("Forward contact tracing" ∪ "backward contact tracing" ∪ "sameAddress")
   # Summarize now
   osOutbreak$n_linkedCasesTotal[i] <- collapseCol(unique(c(getCaseID(osOutbreak$n_sameAddressTotal[i]),  getCaseID(osOutbreak$n_backwardContactTracingTotal[i]) ,  getCaseID(osOutbreak$n_forwardContactTracingTotal[i]))))
   osOutbreak$n_linkedCasesSeqBasedAssesed[i] <-  collapseCol(unique(c(getCaseID(osOutbreak$n_sameAddressSeqBasedAssesed[i]),  getCaseID(osOutbreak$n_backwardContactTracingSeqBasedAssesed[i]) ,  getCaseID(osOutbreak$n_forwardContactTracingSeqBasedAssesed[i]))))
   osOutbreak$n_linkedCasesSeqBasedAssesedAccepted[i] <- collapseCol(unique(c(getCaseID(osOutbreak$n_sameAddressSeqBasedAssesedAccepted[i]),  getCaseID(osOutbreak$n_backwardContactTracingSeqBasedAssesedAccepted[i]) ,  getCaseID(osOutbreak$n_forwardContactTracingSeqBasedAssesedAccepted[i]))))
   
   ## Genetically identical community cases
   n_genIdenticalCommunityCases <- unique(getCaseID(subOut$genLinkedCommunityCasesCaseIDs))
   osOutbreak$n_genIdenticalCommunityCases[i] <- collapseCol(rmCaseIDs(n_genIdenticalCommunityCases))
   n_genIdenticalCommunityCasesNotConnectedViaEpiData <- unique(getCaseID(subOut$genLinkedCommunityCasesnoEdgeCaseIDs))
   osOutbreak$n_genIdenticalCommunityCasesNotConnectedViaEpiData[i] <- collapseCol(rmCaseIDs(n_genIdenticalCommunityCasesNotConnectedViaEpiData))
   
   
   
   ## Nonstructured Linked cases
   healtCareLinkIDs <- c(n_sameAddressTotal, n_backwardContactTracingCommunityInfectedByOutbreak,
                         n_backwardContactTracingOutbreakInfectedByCommunity, n_forwardContactTracingTotal  )
   
   rmCaseIDs2 <-  function(CaseIDVector, target) {
      CaseIDVector[!CaseIDVector %in% target]
   }
   
   n_nonstructuredLinkedCases <- unique(getCaseID(subOut$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredCaseIDs))
   osOutbreak$n_nonstructuredLinkedCases[i] <- collapseCol(rmCaseIDs2(n_nonstructuredLinkedCases,healtCareLinkIDs))
   ac <- unique(getCaseID(subOut$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs))
   rej<- unique(getCaseID(subOut$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenNOTLinkedCaseIDs))
   ac <- rmCaseIDs2(ac,healtCareLinkIDs)
   rej <- rmCaseIDs2(rej,healtCareLinkIDs)
   all <- c(ac, rej)
   osOutbreak$n_nonstructuredLinkedCasesSeqBasedAssesed[i] <- collapseCol(all)
   osOutbreak$n_nonstructuredLinkedCasesSeqBasedAssesedAccepted[i] <- collapseCol(ac)
   
   
   
   
   ## Additionally linked cases manual analysis
   healtCareLinkIDs <- c(n_sameAddressTotal, n_backwardContactTracingCommunityInfectedByOutbreak,
                         n_backwardContactTracingOutbreakInfectedByCommunity, n_forwardContactTracingTotal,
                         n_nonstructuredLinkedCases
   )
   
   n_additionalLinkedSeqConfirmed <- unique(getCaseID(subOut$genLinkedNonContactPersonCaseIDs))
   n_additionalLinkedSeqConfirmed <- rmCaseIDs2(n_additionalLinkedSeqConfirmed,healtCareLinkIDs)
   osOutbreak$n_additionalLinkedSeqConfirmed[i] <-  collapseCol(n_additionalLinkedSeqConfirmed)
   
   
   n_additionalLinkedIndirectSeqBasedEpiLinked <- unique(getCaseID(subOut$linkedNonContactPersonNonSequencedNonOutbreakCaseIDs))
   osOutbreak$n_additionalLinkedIndirectSeqBasedEpiLinked[i] <- collapseCol(rmCaseIDs2(n_additionalLinkedIndirectSeqBasedEpiLinked,healtCareLinkIDs))
   
   n_additionalOtherwiseLinked <- unique(getCaseID(subOut$otherwiseLinkedCaseIDs))
   n_additionalOtherwiseLinked <-  rmCaseIDs2(n_additionalOtherwiseLinked,healtCareLinkIDs)
   osOutbreak$n_additionalOtherwiseLinked[i] <- collapseCol(n_additionalOtherwiseLinked)
   
   
   
   
   
   
   ## Missing outbreak case
   n_missedForwardContactTracingSeqBasedAssesedAccepted <- unique(getCaseID(subOut$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissedCaseID))
   n_missedForwardContactTracingSeqBasedAssesedAccepted <- rmCaseIDs(n_missedForwardContactTracingSeqBasedAssesedAccepted)
   osOutbreak$n_missedForwardContactTracingSeqBasedAssesedAccepted[i] <- collapseCol(n_missedForwardContactTracingSeqBasedAssesedAccepted)
   
   n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted <- unique(getCaseID(subOut$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedMissedCaseID))
   n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted <- rmCaseIDs2(n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted,healtCareLinkIDs)
   osOutbreak$n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted[i] <- collapseCol(n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted)
   
   n_missedAdditionalLinkedSeqConfirmed <- unique(getCaseID(subOut$genLinkedNonContactPersonCaseIDMissed))
   n_missedAdditionalLinkedSeqConfirmed <- rmCaseIDs2(n_missedAdditionalLinkedSeqConfirmed,healtCareLinkIDs)
   osOutbreak$n_missedAdditionalLinkedSeqConfirmed[i] <- collapseCol(n_missedAdditionalLinkedSeqConfirmed)
   
   
   Hmissed <- c(n_missedForwardContactTracingSeqBasedAssesedAccepted,
                n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted,
                n_missedAdditionalLinkedSeqConfirmed)
   
   colH <- c(n_sameAddressTotal, n_backwardContactTracingCommunityInfectedByOutbreak,
             n_backwardContactTracingOutbreakInfectedByCommunity, n_forwardContactTracingTotal)
   
   osOutbreak$n_totalMissingCases[i] <- collapseCol(unique(Hmissed))
   
   n_missedGenIdenticalCommunityCases <- Hmissed[Hmissed %in% colH]
   osOutbreak$n_missedGenIdenticalCommunityCases[i] <- collapseCol(n_missedGenIdenticalCommunityCases)
   
   a1 <- unique(getCaseID(subOut$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs))
   a1 <- rmCaseIDs(a1)
   a2 <- unique(getCaseID(subOut$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs))
   a2 <- rmCaseIDs(a2)
   a3 <- unique(getCaseID(subOut$genLinkedNonContactPersonCaseIDs))
   a3 <- rmCaseIDs(a3)
   
   HNotmissed <- c(a1, a2, a3)
   

   healtCareLinkIDs2 <- c(n_sameAddressTotal, n_backwardContactTracingCommunityInfectedByOutbreak,
                          n_backwardContactTracingOutbreakInfectedByCommunity, n_forwardContactTracingTotal,
                          n_nonstructuredLinkedCases, n_additionalLinkedSeqConfirmed, n_additionalOtherwiseLinked,
                          n_missedGenIdenticalCommunityCases, n_missedForwardContactTracingSeqBasedAssesedAccepted,
                          n_missedNonstructuredLinkedCasesSeqBasedAssesedAccepted,
                          n_missedAdditionalLinkedSeqConfirmed  )
   
   
   n_missedAdditionalLinkedIndirectSeqBasedEpiLinked <- unique(getCaseID(subOut$linkedNonContactPersonNonSequencedNonOutbreakMissed))
   n_missedAdditionalLinkedIndirectSeqBasedEpiLinked <- rmCaseIDs2(n_missedAdditionalLinkedIndirectSeqBasedEpiLinked,healtCareLinkIDs2)
   osOutbreak$n_missedAdditionalLinkedIndirectSeqBasedEpiLinked[i] <- collapseCol(n_missedAdditionalLinkedIndirectSeqBasedEpiLinked)
   
   
   
   n_missedAdditionalOtherwiseLinked <- unique(getCaseID(subOut$casesLinkedByEpidemiologicalDataCaseIDs))
   n_missedAdditionalOtherwiseLinked <- rmCaseIDs2(n_missedAdditionalOtherwiseLinked,unique(Hmissed))
   osOutbreak$n_missedAdditionalOtherwiseLinked[i] <- collapseCol(n_missedAdditionalOtherwiseLinked)
   
   
   
   ##  Linked community cases after manual review with genetic support	
   # J, AB, AC minus C,  AF, AO, AP
   J <- c( unique(getCaseID(subOut$n_positiveSameAddressCasesNotRegisteredSequencedGenLinkedCaseIDs)),
           unique(getCaseID(subOut$n_NotRegisteredInfectedByLinksTotalSequencedGenLinkedCaseIDs)),
           unique(getCaseID(subOut$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs)))
   J <- rmCaseIDs(J)
   
   AB <- unique(getCaseID(subOut$n_nonstructuredPositiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinkedCaseIDs))
   AB <- rmCaseIDs2(AB,healtCareLinkIDs)
   
   AC <- unique(getCaseID(subOut$genLinkedNonContactPersonCaseIDs))
   AC <- rmCaseIDs2(AC,healtCareLinkIDs)
   
   A1 <- unique(c(J, AB, AC))
   
   C  <- subCases
   AF <- Hmissed
   AO <-unique(getCaseID(subOut$linkedNonContactPersonNonSequencedNonOutbreakMissed))
   AO <- rmCaseIDs2(AO,healtCareLinkIDs2)
   AP <- n_missedAdditionalOtherwiseLinked
   
   A2 <- c(C, AF,AO,AP)
   
   osOutbreak$n_linkedCommunityCasesManualReviewSeqBasedAssesedAccepted[i] <- collapseCol(A1[!A1 %in% A2])
   
   
   A3 <- unique(c(J, AB, AC, rmCaseIDs2(n_additionalLinkedIndirectSeqBasedEpiLinked,healtCareLinkIDs)))
   
   osOutbreak$n_linkedCommunityCasesManualReviewSeqBasedAssesedAcceptedPlusIndirectGeneticEvidence[i] <-  collapseCol(A3[!A3 %in% A2])  
   
   
   
   n_erroneouslyAssignedCasesManualReviewSequenced <- unique(getCaseID(subOut$erroneouslyAssignedCasesCaseIDs))
   n_erroneouslyAssignedCasesManualReviewSequenced <- n_erroneouslyAssignedCasesManualReviewSequenced[!n_erroneouslyAssignedCasesManualReviewSequenced %in% subOut$misassignedCasesCaseIDs]
   n_erroneouslyAssignedCasesManualReviewSequenced <- n_erroneouslyAssignedCasesManualReviewSequenced[!n_erroneouslyAssignedCasesManualReviewSequenced %in% subCases[!subCases %in% nodes2$CaseID]]
   if(length(n_erroneouslyAssignedCasesManualReviewSequenced) != 0) osOutbreak$n_erroneouslyAssignedCasesManualReviewSequenced[i] <- collapseCol(n_erroneouslyAssignedCasesManualReviewSequenced[n_erroneouslyAssignedCasesManualReviewSequenced != "NA"])
   
   n_erroneouslyAssignedCasesManualReviewNotSequenced <- unique(getCaseID(subOut$erroneouslyAssignedCasesCaseIDs))
   n_erroneouslyAssignedCasesManualReviewNotSequenced <- n_erroneouslyAssignedCasesManualReviewNotSequenced[!n_erroneouslyAssignedCasesManualReviewNotSequenced %in% subOut$misassignedCasesCaseIDs]
   n_erroneouslyAssignedCasesManualReviewNotSequenced <- n_erroneouslyAssignedCasesManualReviewNotSequenced[!n_erroneouslyAssignedCasesManualReviewNotSequenced %in% subCases[subCases %in% nodes2$CaseID]]
   if(length(n_erroneouslyAssignedCasesManualReviewNotSequenced) != 0)  osOutbreak$n_erroneouslyAssignedCasesManualReviewNotSequenced[i] <- collapseCol(n_erroneouslyAssignedCasesManualReviewNotSequenced[n_erroneouslyAssignedCasesManualReviewNotSequenced != "NA"])
   
   
   eGlI <- as.character(subOut$externalGenLinksInverstigated)
   eGlI <- lapply(eGlI, function(egli){
      gsub(" ", "", strsplit(egli, "\r")[[1]][[1]])
   })
   eGlI[eGlI == "NA"] <- 0
   eGlI <- as.numeric(as.character(eGlI))
   osOutbreak$externalGeneticLinksInvestigated[i] <- sum(eGlI)
   osOutbreak$non_structuredLinksInvestigated[i] <-  sum(subOut$nonstructuredContactsAnalysed)
   
   
}

write.csv2(osOutbreak, file="mock_os_Outbreak.csv", row.names=F)





## Make outbreakLinks table

ol <- data.frame(seqOut=out$outbreakID)

## Generate variable column names
for(i in out$outbreakID){
   
   ol[, i] <- ""
}

edge3 <- edge2[edge2$indexSearch %in% edgesGraph$indexSearch,]
table(edge3$Typ)





for(i in 1:nrow(ol)){
   
   #i <- 39
   sec <- ol$seqOut[i]
   secCases <- outbreaks$Aktenzeichen[outbreaks$AusbruchInfo_NameGA == sec]
   missAssignedCases <- getCaseID(out$misassignedCasesCaseIDs[out$outbreakID == sec])
   secCases <- secCases[!secCases %in% missAssignedCases]
   
   relOut <- names(ol)[-1]
   relOut <- relOut[!relOut %in% sec]
   
   moritzLink <- eva$`Linked to other outbreaks (IDs)`[eva$`Outbreak ID` == sec]
   moritzLink <- toupper(moritzLink)
   moritzLink <- getCaseID(moritzLink)
   
   for(x in relOut){
      
      #x <- names(ol)[-1][[2]]
      pri <- x
      priCases <- outbreaks$Aktenzeichen[outbreaks$AusbruchInfo_NameGA == pri]
      primissAssignedCases <- getCaseID(out$misassignedCasesCaseIDs[out$outbreakID == pri])
      priCases <- priCases[!priCases %in% missAssignedCases]
      
      
      ## Get forward backward sameadress and genetically links
      ## Forward
      outForward <- edge3[(edge3$CaseID_Index %in% secCases & 
                              edge3$CaseID_Kontaktperson %in% priCases) |
                             (edge3$CaseID_Index %in% priCases & 
                                 edge3$CaseID_Kontaktperson %in% secCases),]
      
      
      
      outCont <- outForward[outForward$Typ == "SurvnetKontaktperson",]
      outBack <- outForward[outForward$Typ == "SurvnetAngestecktBei",]
      outAddr <- outForward[outForward$Typ == "SelbeAdresse",]
      
      ## Calculate genetic distance
      outlist <- data.frame(name=1, clonal="NA", n_clonal=0)
      
      cases <- nodes[nodes$CaseID %in% secCases,]
      cases <- cases[cases$SeqID != "",]
      
      conCases <- list()
      
      for(everyCase in  seq_len(nrow(cases))){
         
         #everyCase <- 1
         curCase <- cases[everyCase,]
         
         ##Get best case sequence
         c1 <- curCase$SeqID
         
         allCases_subset <- nodes2[nodes2$CaseID %in% priCases,]
         
         if(nrow(allCases_subset) > 0){
            
            ##Get best case sequence
            c1All <- allCases_subset$SeqID
            
            cc <- expand.grid(c1, c1All)
            cc2 <- expand.grid(curCase$CaseID, allCases_subset$CaseID)
            
            cc$case1 <- as.character(cc2$Var1)
            cc$case2 <- as.character(cc2$Var2)
            
            cc$Var1 <- as.character(cc$Var1)
            cc$Var2 <- as.character(cc$Var2)
            
            cc <- cc[cc$Var1 != cc$Var2,]
            
            cc$var1_inDM <- 0
            cc$var2_inDM <- 0
            
            cc$var1_inDM[cc$Var1 %in% names(dm)] <- 1
            cc$var2_inDM[cc$Var2 %in% names(dm)] <- 1
            
            cc <- cc[cc$var1_inDM == 1 & cc$var2_inDM == 1,]
            
            if(nrow(cc) != 0){
               
               cc$dist <- lapply(1:nrow(cc), function(x){
                  
                  getGeneticDistance(cc$Var1[x], cc$Var2[x])
                  
               })
               
               conCases[[curCase$CaseID]] <-   nrow(cc[cc$dist <=1,])
               
               
            }
         }
      }
      
      
      outlist$clonal[1] <- sum(unlist(conCases))
      
      moritzLinkCur <- 0
      if(pri %in% moritzLink) moritzLinkCur <- 1
      
      moritzLink2 <- eva$`Linked to other outbreaks (IDs)`[eva$`Outbreak ID` == pri]
      moritzLink2 <- toupper(moritzLink2)
      moritzLink2 <- getCaseID(moritzLink2)
      moritzLinkCur2 <- 0
      if(sec %in% moritzLink2) moritzLinkCur2 <- 1
      
      mlc <- 0
      if((moritzLinkCur2 == 1)| (moritzLinkCur == 1))mlc <- 1
      
      output <- paste("Forward:", nrow(outCont), "\n",
                      "Backward:", nrow(outBack), "\n",
                      "SameAddress:", nrow(outAddr), "\n",
                      "Genetically linked:", outlist$clonal[1], "\n",
                      "Manual analysis:", mlc)
      
      if(nrow(outCont) == 0 & nrow(outBack) == 0 & nrow(outAddr)==0 & outlist$clonal[1]==0 & moritzLinkCur==0){
         output <- ""
      }
      
      ol[i,x] <- output
      
   }
   
}


write.csv2(ol, file="mock outbreak links")


## Add outbreak links to os outbreak summary per outbreak and total
osOutbreak$identifiedOutbreakLinks <- ""

for(i in 1:nrow(osOutbreak)){
   
   #curOutID <- osOutbreak$outbreakID[12]
   curOutID <- osOutbreak$outbreakID[i]
   olCur <- ol[, names(ol) %in% c("seqOut",curOutID)]
   olCur <- olCur[olCur$seqOut != curOutID,]
   olCur <- olCur[olCur[,curOutID] != "",]
   
   if(nrow(olCur) > 0){ 
      olCur$ins <- paste0(olCur$seqOut, "\n", olCur[,2])
      
      insert <- paste(olCur$ins, collapse = "\n")
      osOutbreak$identifiedOutbreakLinks[i] <- insert
   }
}


sum(osOutbreak$identifiedOutbreakLinks != "")

osOutbreak$DateEarliestCase <- out$firstDate[match(osOutbreak$outbreakID, out$outbreakID)]
osOutbreak$DateEarliestCase <- substr(osOutbreak$DateEarliestCase, 6,7)

write.csv2(osOutbreak, file="mock_os_Outbreak.csv", row.names=F)




## Add outbreak links to os outbreak summary per outbreak and total
os$identifiedOutbreakLinks <- ""

i <- 1
for(i in 1:nrow(os)){
   
   #curOutID <- osOutbreak$outbreakID[1]
   curOutIDs <- out$outbreakID[out$outbreakType %in% os$category[i]]
   reportCon <- list()
   
   for(outIDs in curOutIDs){
      #outIDs <- "CVD-DÜS-ALT-2021-0133"
      olCur <- ol[, names(ol) %in% c("seqOut",outIDs)]
      olCur <- olCur[olCur$seqOut != outIDs,]
      olCur <- olCur[olCur[,outIDs] != "",]
      
      if(nrow(olCur) > 0){ 
         olCur$ins <- paste0(outIDs, " to ", olCur$seqOut, "\n", olCur[,2])
         
         insert <- paste(olCur$ins, collapse = "SEP")
         reportCon[[outIDs]] <- insert
      }
   }
   
   if(length(reportCon) != 0){
      reportId <- unlist(reportCon)
      reportId <- strsplit(reportId, "SEP")
      reportDF <- data.frame(con=unlist(reportId), nameID1="", nameID2="")
      
      reportDF$nameID1 <- as.character(lapply(1:nrow(reportDF), function(x){
         
         strsplit(reportDF$con[x], " to ")[[1]][[1]]
         
      }))
      
      reportDF$nameID2 <- as.character(lapply(1:nrow(reportDF), function(x){
         
         y <-  strsplit(reportDF$con[x], " to ")[[1]][[2]]
         y <- strsplit(y, "\n")[[1]][[1]]
         
      }))
      
      reportDF$nameIDs <- as.character(lapply(1:nrow(reportDF), function(x){
         
         y <- reportDF$nameID1[x]
         y2 <- reportDF$nameID2[x]
         z <- sort(c(y, y2))
         paste(z, collapse = " ")
         
      }))
      
      reportDF$nameIDsSearch <- as.character(lapply(1:nrow(reportDF), function(x){
         
         y <- reportDF$nameID1[x]
         y2 <- reportDF$nameID2[x]
         paste(c(y,y2), collapse = " ")
         
      }))
      
      reportDF$dup <- duplicated(reportDF$nameIDs)
      reportDF <- reportDF[!reportDF$dup,]
      
      reportDF$genEpi <- 0
      reportDF$gen <- 0
      reportDF$epi <- 0
      reportDF$mor <- 0
      
      for(epiGen in 1:nrow(reportDF)){
         epiGen2 <- reportDF$con[epiGen]
         y <- strsplit(epiGen2, "\n")[[1]]
         y <- gsub(" ", "", y)
         y <- y[-1]
         y <- as.numeric(lapply(y, function(yl1){ strsplit(yl1, ":")[[1]][[2]] }))
         reportDF$epi[epiGen]  <-  y[[1]]+y[[2]]+y[[3]]
         
      }
      
      
      for(epiGen in 1:nrow(reportDF)){
         epiGen2 <- reportDF$con[epiGen]
         y <- strsplit(epiGen2, "\n")[[1]]
         y <- gsub(" ", "", y)
         y <- y[-1]
         y <- as.numeric(lapply(y, function(yl1){ strsplit(yl1, ":")[[1]][[2]] }))
         reportDF$mor[epiGen]  <- y[[5]]
         
      }
      
      
      for(epiGen in 1:nrow(reportDF)){
         epiGen2 <- reportDF$con[epiGen]
         y <- strsplit(epiGen2, "\n")[[1]]
         y <- gsub(" ", "", y)
         y <- y[-1]
         y <- as.numeric(lapply(y, function(yl1){ strsplit(yl1, ":")[[1]][[2]] }))
         reportDF$gen[epiGen]  <- y[[4]]
         
      }
      
      reportDF$genEpi[reportDF$gen != 0 & (reportDF$epi != 0 | reportDF$mor != 0)] <- 1
      
      reportDF$category <- ""
      reportDF$category <- as.character(lapply(1:nrow(reportDF), function(testitest){
         
         morCur <-  reportDF$mor[testitest]
         epiCur <-  reportDF$epi[testitest]
         genCur <- reportDF$gen[testitest]
         
         if(morCur > 0) morCur <- 1
         if(epiCur > 0) epiCur <- 1 
         if(genCur > 0) genCur <- 1
         
         paste0("[Epidemiological and direct or indirect genetic link detected/conformed in manual analysis: ",
                morCur,
                "; Epidemiological link in routine data: ",
                epiCur,
                "; Direct genetic link: ",
                genCur,
                "]")
         
         
      }))
      
      reportDFTable <- data.frame(table(reportDF$category))
      
      #Define needed functions
      Numextract <- function(string){
         unlist(regmatches(string, gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
      }
      
      reportDFTable$out <- paste0(reportDFTable$Freq, "x ",reportDFTable$Var1)
      reportDFTable$sum2 <- as.character(lapply(reportDFTable$Var1, function(OutbreakLinkCategory){ paste(Numextract(OutbreakLinkCategory), collapse = "" ) }))
      reportDFTable <- reportDFTable[order(reportDFTable$sum2, decreasing = T),]
      
      
      os$identifiedOutbreakLinks[i] <- paste(reportDFTable$out , collapse="\n")
   }
}


write.csv2(os, file="mock_os.csv", row.names=F)

os$category[os$category =="Other care home"] <- "Residential Care Facility"
osOutbreak$category[osOutbreak$category =="Other care home"] <- "Residential Care Facility"


write.csv2(os, file="mock_os.csv", row.names=F)

write.csv2(osOutbreak, file="mock_os_Outbreak.csv", row.names=F)

write.csv2(ol, file="mock_ol.csv", row.names=F)

##convert CaseIDs to number of caseIDs
osOutbreak2 <- osOutbreak

library(stringr)

for(i in names(osOutbreak2)[!names(osOutbreak2) %in% c("identifiedOutbreakLinks","outbreakID")]){
   
   test <- osOutbreak2[[i]]
   if(any(str_detect(test, "CVD"))){
      
      count <- as.numeric(lapply(test, function(x){
         length(strsplit(x, ",")[[1]])
      }))
 
      osOutbreak2[[i]] <-   count
   }
   
   
}

write.csv2(osOutbreak2, file="mock_os_Outbreak.csv", row.names=F)


save.image("mock FinalData2.RData")


load("mock FinalData2.RData")


#### Outbreak main figure
 
#### For 30% of outbreaks, 2 or more cases had sequencing data available
sum(osOutbreak$n_outbreaksDistanceAvailable=="TRUE")/nrow(osOutbreak)

os$clonal <- os$n_outbreaksDistanceAvailable - os$n_outbreaksNonClonal

out4 <- data.frame(Freq=os$n_outbreaks, Var1=os$category, Var2="")
out2 <- data.frame(Freq=os$n_outbreaksDistanceAvailable, Var1=os$category, Var2="At least 1 distance available")
out3 <- data.frame(Freq=os$clonal, Var1=os$category, Var2="clonal")

outAll <- rbind(out4, out2, out3)


outAll$total <- out4$Freq[match(outAll$Var1, out4$Var1)]
out2avail <- out2[out2$Var2 == "At least 1 distance available",]
outAll$total[outAll$Var2 == "clonal"] <- out2avail$Freq[match(outAll$Var1[outAll$Var2 == "clonal"],
                                                              out2avail$Var1 )]

outAll$percentage <- round((outAll$Freq/outAll$total)*100)
outAll$percentage <- as.character(paste0(outAll$percentage, "%"))

outAll$percentage[outAll$Var2 == ""]  <- ""

outAll$Var1 <- factor(outAll$Var1, levels=out4$Var1[order(out4$Freq, decreasing = T)])

outAll$Var2[outAll$Var2 == ""] <- "All reported outbreaks"
outAll$Var2[outAll$Var2 == "clonal"] <- "Outbreak clonal"

replaceVec <- outAll$Var2[outAll$Var2 == "At least 1 distance available"]
outAll$Var2[outAll$Var2 == "At least 1 distance available"] <- paste0(replaceVec, "\n(% of all outbreaks)")

replaceVec <- outAll$Var2[outAll$Var2 == "Outbreak clonal"]
outAll$Var2[outAll$Var2 == "Outbreak clonal"] <- paste0(replaceVec, "\n(% of outbreaks with gen. distances)")

outAll$percentage[outAll$percentage == "NaN%"] <- ""

outAll$Var1 <- as.character(outAll$Var1)

factorVector <- outAll[outAll$Var2 == "All reported outbreaks",]
factorVector <- factorVector$Var1[order(factorVector$Freq, decreasing = T)]

outAll$Var1 <- factor(outAll$Var1, levels=factorVector)
outAll$percentage[outAll$percentage == "0%"] <- ""

outAll$vjust <- -.1

outAll$vjust[outAll$Var2 == "At least 1 distance available\n(% of all outbreaks)"] <- -.1
outAll$vjust[outAll$Var2 == "Outbreak clonal\n(% of outbreaks with gen. distances)"] <- 1.1


global_size <- 15

#Figure Outbreaks:
library(ggplot2)
g<- ggplot(data=outAll, aes(x=Var1, y=Freq, fill=Var2)) +
   geom_bar(stat="identity",position = "identity")+ 
   theme(axis.text.x = element_text(angle = 45, hjust=1,size=13))+
   xlab("")+ylab("Number of outbreaks")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+  theme(text = element_text(size=global_size))+ 
   geom_text(aes(label = percentage),  vjust = outAll$vjust, hjust= .5, size=4)
g

##ggsave(g, filename="Figure Outbreaks (A).svg", width= 8.5, height = 5.5)
#ggsave(g, filename="Figure 4 A.svg", width= 8.5, height = 5.5)



# B) Cases by outbreak type, % sequenced, how many rejected. 

os$clonalCases <- os$n_sequencedCasesInoutbreaksDistanceAvailable - os$n_erroneousCases

out4 <- data.frame(freq=os$n_cases, outbreak=os$category, type= "All reported cases")
out2 <- data.frame(freq=os$n_sequencedCasesInoutbreaksDistanceAvailable, outbreak=os$category, type="Sequenced cases")
out3 <- data.frame(freq=os$clonalCases, outbreak=os$category, type="Accepted cases")

out <- rbind(out4, out2, out3)



out$percentage <- 0
out$percentage[out$type == "Sequenced cases"] <- round(out$freq[out$type == "Sequenced cases"]/out$freq[out$type == "All reported cases"]*100)
out$percentage[out$type == "Accepted cases"] <- round(out$freq[out$type == "Accepted cases"]/out$freq[out$type == "Sequenced cases"]*100)

out$percentage <- as.character(paste0(out$percentage, "%"))
out$percentage[out$type == "All reported cases"] <- ""

#factor <- out[out$type ==  "All reported cases",]

out$outbreak <- as.character(out$outbreak)

outbreakOrder <- outAll[outAll$Var2 == "All reported outbreaks",]
outbreakOrder <- outbreakOrder[order(outbreakOrder$Freq, decreasing = T),]
out$outbreak <- factor(out$outbreak, levels=  outbreakOrder$Var1 )

out$type <- as.character(out$type)
replaceVec <- out$type[out$type == "Sequenced cases"]
out$type[out$type == "Sequenced cases"] <- paste0(replaceVec, "\n(% of all cases)")

replaceVec <- out$type[out$type == "Accepted cases"]
out$type[out$type == "Accepted cases"] <- paste0(replaceVec, "\n(% of sequenced cases)")

out$type <- factor(out$type, levels=c("All reported cases","Sequenced cases\n(% of all cases)","Accepted cases\n(% of sequenced cases)" ))
out$percentage[out$percentage == "Inf%"] <- "0%"
out$percentage[out$percentage == "NaN%"] <- ""
out$percentage[out$percentage == "0%"] <- ""
out$percentage[out$percentage == "100%"] <- ""

out$vjust <- -.1

out$vjust[out$type == "Sequenced cases\n(% of all cases)"] <- -.1
out$vjust[out$type == "Accepted cases\n(% of sequenced cases)"] <- 1.1



g<- ggplot(data=out, aes(x=outbreak, y=freq, fill=type)) +
   geom_bar(stat="identity",position = "identity")+ 
   theme(axis.text.x = element_text(angle = 45, hjust=1,size=13))+
   xlab("")+ylab("Number of cases")+theme(legend.title=element_blank())+
   geom_text(aes(label = percentage), vjust = out$vjust, size=4)+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=global_size))

library(ggbreak)
g <- g+ scale_y_break(c(162, 227))
g <- g  + scale_y_continuous(breaks = c(50,100,150,160, 230))

g <- g+theme(
   axis.ticks.y.right = element_blank(), # Remove ticks on the right y-axis
   axis.text.y.right = element_blank()  # Optional: remove text labels on the right y-axis
)
g
##ggsave(g, filename="Figure Outbreaks (B).svg", width= 8, height = 5.7)
#ggsave(g, filename="Figure 4 B.svg", width= 8, height = 5.7)






# C) Contact tracing/ infected by links by outbreak (rejected and accepted)

out4 <- data.frame(freq=os$n_linkedCasesTotal, outbreak=os$category, type= "Epidemiologically linked cases")
out2 <- data.frame(freq=os$n_linkedCasesSeqBasedAssesed, outbreak=os$category,
                   type="... with distance available")
out3 <- data.frame(freq= os$n_linkedCasesSeqBasedAssesedAccepted,  outbreak=os$category,
                   type="... accepted based on distance")



# out3$freq <- out3$i - out3$freq
# out3$i <- NULL

out <- rbind(out4, out2, out3)



sum(out2$freq)
sum(out3$freq)

out$percentage <- 0
out$percentage[out$type == "... with distance available"] <- round(out$freq[out$type == "... with distance available"]/out$freq[out$type == "Epidemiologically linked cases"]*100)
out$percentage[out$type =="... accepted based on distance"] <- round(out$freq[out$type == "... accepted based on distance"]/out$freq[out$type == "... with distance available"]*100)

out$percentage <- as.character(paste0(out$percentage, "%"))
out$percentage[out$type == "Contact tracing links"] <- ""

#factor <- out[out$type ==  "Contact tracing links",]

out$type <- as.character(out$type)
replaceVec <- out$type[out$type == "... with distance available"]
out$type[out$type == "... with distance available"] <- paste0(replaceVec, "\n(% of links)")

replaceVec <- out$type[out$type == "... accepted based on distance"]
out$type[out$type == "... accepted based on distance"] <- paste0(replaceVec, "\n(% of links with genetic distance)")

out$type <- factor(out$type, levels=c("Epidemiologically linked cases"  ,
                                      "... with distance available\n(% of links)",
                                      "... accepted based on distance\n(% of links with genetic distance)" ))

out$percentage[out$percentage == "NaN%"] <- "0%"
out$percentage[out$percentage == "0%"] <- ""

out$outbreak <- as.character(out$outbreak)
out$outbreak[out$outbreak == "Care home"] <- "Care\nhome"
out$outbreak[out$outbreak == "Recreational context"] <- "Recreational\ncontext"
out$outbreak[out$outbreak == "Residential Care Facility"] <- "Residential\nCare Facility"
out$outbreak[out$outbreak == "Refugee Accommodation"] <- "Refugee\nAccommodation"
out$outbreak[out$outbreak == "Kindergarten/daycare"] <- "Kindergarten/\ndaycare"

out$outbreak <- factor(out$outbreak, levels= c("School", "Kindergarten/\ndaycare", "Hospital", "Care\nhome", 
                                               "Work", "Nightlife" ,"Residential\nCare Facility",
                                               "Recreational\ncontext", "Refugee\nAccommodation", "Other") )

g<- ggplot(data=out, aes(x=outbreak, y=freq, fill=type)) +
   geom_bar(stat="identity",position = "dodge2")+ 
   #geom_bar(stat="identity",position = "identity")+ 
   #theme(axis.text.x = element_text(angle = 45, hjust=1,size=13))+
   xlab("")+ylab("Epidemiologically linked community cases")+theme(legend.title=element_blank())+
   geom_text(aes(label = percentage), vjust = -0.2, size=4, position = position_dodge(width = .9))+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=global_size))


library(ggbreak)
g <- g+ scale_y_break(c(70,143, 145, 290))
g <- g  + scale_y_continuous(breaks = c(10,20,30,40,50, 70,140,144, 291))

g <- g+theme(
   axis.ticks.y.right = element_blank(), # Remove ticks on the right y-axis
   axis.text.y.right = element_blank()  # Optional: remove text labels on the right y-axis
)
g
##ggsave(g, filename="Figure Outbreaks (C).svg", width= 14.1, height = 5)
#ggsave(g, filename="Figure 4 C.svg", width= 14.1, height = 5)




## Figure Contact Tracing (D)
out1 <- data.frame(freq=os$n_genIdenticalCommunityCases, outbreak=os$category, type = "Genetically linked cases")
out2 <- data.frame(freq2=os$n_genIdenticalCommunityCases, freq=os$n_genIdenticalCommunityCasesNotConnectedViaEpiData, 
                   outbreak=os$category, type = "... with epidemiological link")
out2$freq <- out2$freq2 - out2$freq
out2$freq2 <- NULL

out <- rbind(out1, out2)

out$percentage <- 0
out$percentage[out$type == "... with epidemiological link"] <- round(out$freq[out$type == "... with epidemiological link"]/out$freq[out$type == "Genetically linked cases"]*100)


out$percentage <- as.character(paste0(out$percentage, "%"))
out$percentage[out$type == "Genetically linked cases"] <- ""


out$type <- as.character(out$type)
replaceVec <- out$type[out$type == "... with epidemiological link"]
out$type[out$type == "... with epidemiological link"] <- paste0(replaceVec, "\n(% of links)")

out$type <- factor(out$type, levels=c("Genetically linked cases"  ,
                                      "... with epidemiological link\n(% of links)"))

out$percentage[out$percentage == "NaN%"] <- "0%"
out$percentage[out$percentage == "0%"] <- ""


out$outbreak <- factor(out$outbreak, levels=   c("School", "Kindergarten/daycare", "Hospital", "Care home", 
                                                 "Work", "Nightlife" ,"Residential Care Facility",
                                                 "Recreational context", "Refugee Accommodation", "Other") )

out$vjust <- -.1

out$vjust[out$type == "... with epidemiological link\n(% of links)"] <- -.1


g<- ggplot(data=out, aes(x=outbreak, y=freq, fill=type)) +
   geom_bar(stat="identity",position = "identity")+ 
   xlab("")+ylab("Number of genetically linked cases")+theme(legend.title=element_blank())+
   theme(axis.text.x = element_text(angle = 45, hjust=1,size=13))+
   geom_text(aes(label = percentage), vjust = out$vjust, size=4)+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=global_size))

g


##ggsave(g, filename=paste0("Figure Outbreaks (D).svg"), width=9, height = 5.55)
getwd()
#ggsave(g, filename="Figure 4 D.svg", width=9, height = 5.55)






## check genetic distance of sameAddress and sameAddressSameName
edge4 <- edge3
edge4$index1Seq <- nodes$SeqID[match(edge4$index1, nodes$CaseID)]
edge4$index2Seq <- nodes$SeqID[match(edge4$index2, nodes$CaseID)]


edge4 <- edge4[edge4$Typ %in% c("SelbeAdresse", "SelbeAdresseNachname"),]
edge4 <- edge4[edge4$index1Seq != "" & edge4$index2Seq != "",]

edge4$distance <- as.numeric(lapply(1:nrow(edge4), function(x){
   
   getGeneticDistance(edge4$index1Seq[x], edge4$index2Seq[x])
   
}))

edge4$distanceGroup <- edge4$distance  
edge4$distanceGroup[edge4$distance > 5] <- ">5"

edge4$typ2 <- edge4$Typ 
edge4$indexSearch <- paste(edge4$index1, edge4$index2)
edge4$typ2[!edge4$indexSearch %in% edge4$indexSearch[edge4$typ2 == "SelbeAdresseNachname"] ] <- "SameAddressDifferentName"

edge4$c <- 1
edge4TBL <- tapply(edge4$c[edge4$typ2 == "SameAddressDifferentName"], edge4$distanceGroup[edge4$typ2 == "SameAddressDifferentName" ], sum)
diffName <- data.frame(edge4TBL)
diffName$dist <- row.names(diffName)
diffName$percent <- round(diffName$edge4TBL / sum(diffName$edge4TBL)*100)
diffName$type <- "SameAddressDifferentName"

edge4TBL <- tapply(edge4$c[edge4$typ2 == "SelbeAdresseNachname"], edge4$distanceGroup[edge4$typ2 == "SelbeAdresseNachname" ], sum)
SameName <- data.frame(edge4TBL)
SameName$dist <- row.names(SameName)
SameName$percent <- round(SameName$edge4TBL / sum(SameName$edge4TBL)*100)
SameName$type <- "SelbeAdresseNachname"


all <- rbind(SameName, diffName)


all$percent <- paste0(all$percent, "%")

all$dist <- factor(all$dist, levels=c("0","1","2","3","4","5", ">5"))


## Use correct labels
g<- ggplot(data=all, aes(x= dist, y=edge4TBL, fill=type )) +
   geom_bar(stat="identity",position = "dodge2")+ 
   # theme(axis.text.x = element_text(angle = 45, hjust=1,size=12))+
   xlab("")+ylab("Number of genetically linked cases")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2") +
   #geom_dotplot(binaxis='y', stackdir='center', dotsize=.4, fill="black")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=global_size))+
   geom_text(aes(label = percent), vjust = -0.2, size=4,  position = position_dodge(width = .9))
g
##ggsave(g, filename=paste0("Supplementary Figure ContactTracing Genetic distance sameAdress Name.svg"), width=9, height = 5)
#ggsave(g, filename=paste0("Supplementary Figure 12.svg"), width=9, height = 5)
getwd()

## test
edge4 <- edge3
edge4$index1Seq <- nodes$SeqID[match(edge4$index1, nodes$CaseID)]
edge4$index2Seq <- nodes$SeqID[match(edge4$index2, nodes$CaseID)]


edge4 <- edge4[edge4$Typ %in% c("SelbeAdresse", "SelbeAdresseNachname"),]
edge4 <- edge4[edge4$index1Seq != "" & edge4$index2Seq != "",]

edge4$distance <- as.numeric(lapply(1:nrow(edge4), function(x){
   
   getGeneticDistance(edge4$index1Seq[x], edge4$index2Seq[x])
   
}))

edge4$distanceGroup <- edge4$distance  
edge4$distanceGroup[edge4$distance > 1] <- ">1"
edge4$distanceGroup[edge4$distance <2] <- "<2"

edge4$typ2 <- edge4$Typ 
edge4$indexSearch <- paste(edge4$index1, edge4$index2)
edge4$typ2[!edge4$indexSearch %in% edge4$indexSearch[edge4$typ2 == "SelbeAdresseNachname"] ] <- "SameAddressDifferentName"

edge4$c <- 1
edge4TBL <- tapply(edge4$c[edge4$typ2 == "SameAddressDifferentName"], edge4$distanceGroup[edge4$typ2 == "SameAddressDifferentName" ], sum)
diffName <- data.frame(edge4TBL)
diffName$dist <- row.names(diffName)
diffName$percent <- round(diffName$edge4TBL / sum(diffName$edge4TBL)*100)
diffName$type <- "SameAddressDifferentName"

edge4TBL <- tapply(edge4$c[edge4$typ2 == "SelbeAdresseNachname"], edge4$distanceGroup[edge4$typ2 == "SelbeAdresseNachname" ], sum)
SameName <- data.frame(edge4TBL)
SameName$dist <- row.names(SameName)
SameName$percent <- round(SameName$edge4TBL / sum(SameName$edge4TBL)*100)
SameName$type <- "SelbeAdresseNachname"


all <- rbind(SameName, diffName)


counts <- matrix(c(1527, 299, 1015, 367), 
                 nrow = 2, 
                 byrow = FALSE,
                 dimnames = list(
                    dist = c("<2", ">1"),
                    type = c("SelbeAdresseNachname", "SameAddressDifferentName")
                 ))

# Print the table to verify
test <- chisq.test(counts)

test$statistic
test$parameter

1# X-squared = 48.949


# -	Supplementary Figure Outbreaks
# Supplementary Figure 15
# A	Outbreak sizes (in cases per individual outbreak), by outbreak type 
load("mock_out")

out$outbreak <- as.character(out$outbreakType)
out$outbreak[out$outbreak == "Care home"] <- "Care\nhome"
out$outbreak[out$outbreak == "Recreational context"] <- "Recreational\ncontext"
out$outbreak[out$outbreak == "Residential Care Facility"] <- "Residential\nCare Facility"
out$outbreak[out$outbreak == "Refugee Accommodation"] <- "Refugee\nAccommodation"
out$outbreak[out$outbreak == "Kindergarten/daycare"] <- "Kindergarten/\ndaycare"

out$outbreak <- factor(out$outbreak, levels= c("School", "Kindergarten/\ndaycare", "Hospital", "Care\nhome", 
                                               "Work", "Nightlife" ,"Residential\nCare Facility",
                                               "Recreational\ncontext", "Refugee\nAccommodation", "Other") )


library(ggplot2)
g<- ggplot(data=out, aes(x=outbreak , y=n_casesTotal)) +
   geom_boxplot()+  
   # theme(axis.text.x = element_text(angle = 45, hjust=1,size=12))+
   xlab("")+ylab("Outbreak size")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")  + theme(legend.position = "none")+
   # geom_dotplot(binaxis='y', stackdir='center', dotsize=.4, fill="black")+
   scale_fill_brewer(palette="Set2")
   
g

##ggsave(g, filename=paste0("Supplementary Figure Outbreaks A (size).svg"), width=8, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 15 A.svg"), width=8, height = 4)
getwd()


# B	Box plots over outbreak lengths by outbreak type
out$Duration_days <- out$Duration_days + 1
out$type3 <- out$outbreakType

library(ggplot2)
g<- ggplot(data=out, aes(x=outbreak , y=Duration_days)) +
   geom_boxplot()+  
   # theme(axis.text.x = element_text(angle = 45, hjust=1,size=12))+
   xlab("")+ylab("Outbreak duration in days")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")  + theme(legend.position = "none")+
   # geom_dotplot(binaxis='y', stackdir='center', dotsize=.4, fill="black")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=10))

g

##ggsave(g, filename=paste0("Supplementary Figure Outbreaks B (duration).svg"), width=8, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 15 B.svg"), width=8, height = 4)
getwd()


#C	Pre-Graph: Box plot over D?sseldorf contacts per case in outbreak, by outbreak type (box plot?)
g<- ggplot(data=out, aes(x=outbreak , y=n_contactPersonsDuesseldorf)) +
   geom_boxplot()+  
   # theme(axis.text.x = element_text(angle = 45, hjust=1,size=12))+
   xlab("")+ylab("Number of forward contact tracing records per outbreak")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")  + theme(legend.position = "none")+
   # geom_dotplot(binaxis='y', stackdir='center', dotsize=.4, fill="black")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=10))

g

##ggsave(g, filename=paste0("Supplementary Figure Outbreaks C (n contacts).svg"), width=8, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 15 C.svg"), width=8, height = 4)
getwd()


# 
# #D	Edges: Positive contact tracing records [EDGES, NOT CASES] 
# per outbreak, by outbreak type (box plot?)
g<- ggplot(data=out, aes(x=outbreak , y=n_positiveContactPersonsDuesseldorfNotRegisteredLinks)) +
   geom_boxplot()+
   # theme(axis.text.x = element_text(angle = 45, hjust=1,size=12))+
   xlab("")+ylab("Number of forward contact tracing links per outbreak")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")  + theme(legend.position = "none")+
   # geom_dotplot(binaxis='y', stackdir='center', dotsize=.4, fill="black")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=10))

g

##ggsave(g, filename=paste0("Supplementary Figure Outbreaks D (n pos contact links).svg"), width=8, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 15 D.svg"), width=8, height = 4)
getwd()


# E	Edges: Box plot over positive contact tracing records  [EDGES, NOT CASES],
# per case, by outbreak type (box plot?)
out$posConPerCase <- out$n_positiveContactPersonsDuesseldorfNotRegisteredLinks / out$n_casesDuesseldorf
out$type3 <- out$outbreak
g<- ggplot(data=out, aes(x=outbreak , y=posConPerCase)) +
   geom_boxplot()+
   # theme(axis.text.x = element_text(angle = 45, hjust=1,size=12))+
   xlab("")+ylab("Number of forward contact tracing links per case in outbreak")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")  + theme(legend.position = "none")+
   # geom_dotplot(binaxis='y', stackdir='center', dotsize=.4, fill="black")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=10))

g

##ggsave(g, filename=paste0("Supplementary Figure Outbreaks E (n pos contact links per case).svg"), width=8, height = 4.5)
#ggsave(g, filename=paste0("Supplementary Figure 15 E.svg"), width=8, height = 4.5)
getwd()




# F	Cases: Box plot over positive contacts [CASES, NOT EDGES] per outbreak, by outbreak type

out2 <- data.frame(type3 = out$type3, posKP=out$n_positiveContactPersonsDuesseldorfNotRegistered,
                   posKPseq=out$n_positiveContactPersonsDuesseldorfNotRegisteredSequenced,
                   posKPlinked=out$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinked,
                   posPKnotlinked=0, posKPnoLinkInfo=0)

out2$posPKnotlinked <- out2$posKPseq - out2$posKPlinked
out2$posKPnoLinkInfo <- out2$posKP - out2$posKPlinked

linked <- data.frame(type3=out2$type3, count=out2$posKPlinked, type= "Accepted")
NotLinked <- data.frame(type3=out2$type3, count=out2$posPKnotlinked, type= "Rejected")
NoInfo <- data.frame(type3=out2$type3, count=out2$posKPnoLinkInfo, type= "No distance available")

out3 <- rbind(linked, NotLinked, NoInfo)
out3$type <- factor(out3$type, levels=c("Accepted", "Rejected", "No distance available"))

g<- ggplot(data=out3, aes(x=type3 , y=count, fill=type, color=type)) +
   geom_boxplot()+
   xlab("")+ylab("Number of forward contact tracing links")+theme(legend.title=element_blank())+
   #geom_dotplot(binaxis='y', stackdir='center', dotsize=.4, fill="black")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=10))+
   scale_fill_manual(name= "Clarity", values = c("#66C2A5", "#FC8D62", "#8DA0CB"))+
   scale_color_manual(name = "Clarity", values = c("#57A58CFF", "#D67853FF",  "#7888ADFF"))


g

##ggsave(g, filename=paste0("Supplementary Figure Outbreaks F (n pos contacts).svg"), width=10, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 15 F.svg"), width=10, height = 4)
getwd()


# G	Cases: Box plot over positive contacts [CASES, NOT EDGES]
# per case in outbreak, by outbreak type (box plot?) 
out2 <- data.frame(type3 = out$type3, posKP=out$n_positiveContactPersonsDuesseldorfNotRegistered,
                   posKPseq=out$n_positiveContactPersonsDuesseldorfNotRegisteredSequenced,
                   posKPlinked=out$n_positiveContactPersonsDuesseldorfNotRegisteredSequencedGenLinked,
                   posPKnotlinked=0, posKPnoLinkInfo=0, size=out$n_casesDuesseldorf)

out2$posPKnotlinked <- out2$posKPseq - out2$posKPlinked
out2$posKPnoLinkInfo <- out2$posKP - out2$posKPlinked

linked <- data.frame(type3=out2$type3, count=out2$posKPlinked, type= "Accepted", size=out2$size)
NotLinked <- data.frame(type3=out2$type3, count=out2$posPKnotlinked, type= "Rejected", size=out2$size)
NoInfo <- data.frame(type3=out2$type3, count=out2$posKPnoLinkInfo, type= "No distance available", size=out2$size)

out3 <- rbind(linked, NotLinked, NoInfo)
out3$count <- out3$count/out3$size
#out3 <- out3[out3$count != 0,]
out3$type <- factor(out3$type, levels=c("Accepted", "Rejected", "No distance available"))

g<- ggplot(data=out3, aes(x=type3 , y=count, fill=type, color=type)) +
   geom_boxplot()+  
   # theme(axis.text.x = element_text(angle = 45, hjust=1,size=12))+
   xlab("")+ylab("Number of forward contact tracing links per case per outbreak")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2") +
   # geom_dotplot(binaxis='y', stackdir='center', dotsize=.4, fill="black")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=10))+
   scale_fill_manual(name= "Clarity", values = c("#66C2A5", "#FC8D62", "#8DA0CB"))+
   scale_color_manual(name = "Clarity", values = c("#57A58CFF", "#D67853FF",  "#7888ADFF"))


g

##ggsave(g, filename=paste0("Supplementary Figure Outbreaks G (n pos contacts).svg"), width=9, height = 4.5)
#ggsave(g, filename=paste0("Supplementary Figure 15 G.svg"), width=9, height = 4.5)
getwd()





# H	Edges: Box plot over SameAddress records to other positive 
# cases per outbreak, by outbreak type (box plot?) 

g<- ggplot(data=out, aes(x=type3 , y=n_positiveSameAddressCasesNotRegistered)) +
   geom_boxplot()+
   # theme(axis.text.x = element_text(angle = 45, hjust=1,size=12))+
   xlab("")+ylab("Number of same address links per outbreak")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")  + theme(legend.position = "none")+
   # geom_dotplot(binaxis='y', stackdir='center', dotsize=.4, fill="black")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=10))

g

# #ggsave(g, filename=paste0("Supplementary Figure Outbreaks H (n pos address links).svg"), width=8, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 15 H.svg"), width=8, height = 4)
getwd()


# I	Edges: Box plot over SameAddress records to other
# positive cases, per case in outbreak, by outbreak type (box plot?) 
out$norm_n_positiveSameAddressCasesNotRegistered <- out$n_positiveSameAddressCasesNotRegistered/out$n_casesDuesseldorf
g<- ggplot(data=out, aes(x=type3 , y=norm_n_positiveSameAddressCasesNotRegistered)) +
   geom_boxplot()+
   # theme(axis.text.x = element_text(angle = 45, hjust=1,size=12))+
   xlab("")+ylab("Number of same address links per case per outbreak")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")  + theme(legend.position = "none")+
   # geom_dotplot(binaxis='y', stackdir='center', dotsize=.4, fill="black")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=10))

g

##ggsave(g, filename=paste0("Supplementary Figure Outbreaks I (n pos address per case).svg"), width=8, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 15 I.svg"), width=8, height = 4)
getwd()


# J	Edges: SameAddress records to other positive cases, by outbreak type (box plot?) 

out2 <- data.frame(type3 = out$type3, posKP=out$n_positiveSameAddressCasesNotRegistered,
                   posKPseq=out$n_positiveSameAddressCasesNotRegisteredSequenced,
                   posKPlinked=out$n_positiveSameAddressCasesNotRegisteredSequencedGenLinked,
                   posPKnotlinked=0, posKPnoLinkInfo=0)

out2$posPKnotlinked <- out2$posKPseq - out2$posKPlinked
out2$posKPnoLinkInfo <- out2$posKP - out2$posKPlinked

linked <- data.frame(type3=out2$type3, count=out2$posKPlinked, type= "Accepted")
NotLinked <- data.frame(type3=out2$type3, count=out2$posPKnotlinked, type= "Rejected")
NoInfo <- data.frame(type3=out2$type3, count=out2$posKPnoLinkInfo, type= "No distance available")

out3 <- rbind(linked, NotLinked, NoInfo)
out3 <- out3[out3$count != 0,]
out3$type <- factor(out3$type, levels=c("Accepted", "Rejected", "No distance available"))

g<- ggplot(data=out3, aes(x=type3 , y=count, fill=type, color=type)) +
   geom_boxplot()+  
   # theme(axis.text.x = element_text(angle = 45, hjust=1,size=12))+
   xlab("")+ylab("Number of same address records")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2") +
   # geom_dotplot(binaxis='y', stackdir='center', dotsize=.4, fill="black")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=10))+
   scale_fill_manual(name= "Clarity", values = c("#66C2A5", "#FC8D62", "#8DA0CB"))+
   scale_color_manual(name = "Clarity", values = c("#57A58CFF", "#D67853FF",  "#7888ADFF"))


g

##ggsave(g, filename=paste0("Supplementary Figure Outbreaks J (n pos address).svg"), width=10, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 15 J.svg"), width=10, height = 4)
getwd()


# K	Cases: Box plot over SameAddress positive linked cases per outbreak, by outbreak type (box plot?) 

out2 <- data.frame(type3 = out$type3, posKP=out$n_positiveSameAddressCasesNotRegistered,
                   posKPseq=out$n_positiveSameAddressCasesNotRegisteredSequenced,
                   posKPlinked=out$n_positiveSameAddressCasesNotRegisteredSequencedGenLinked,
                   posPKnotlinked=0, posKPnoLinkInfo=0, size=out$n_casesDuesseldorf)

out2$posPKnotlinked <- out2$posKPseq - out2$posKPlinked
out2$posKPnoLinkInfo <- out2$posKP - out2$posKPlinked

linked <- data.frame(type3=out2$type3, count=out2$posKPlinked, type= "Accepted", size=out2$size)
NotLinked <- data.frame(type3=out2$type3, count=out2$posPKnotlinked, type= "Rejected", size=out2$size)
NoInfo <- data.frame(type3=out2$type3, count=out2$posKPnoLinkInfo, type= "No distance available", size=out2$size)

out3 <- rbind(linked, NotLinked, NoInfo)
out3$count <- out3$count/out3$size
out3 <- out3[out3$count != 0,]
out3$type <- factor(out3$type, levels=c("Accepted", "Rejected", "No distance available"))

g<- ggplot(data=out3, aes(x=type3 , y=count, fill=type, color=type)) +
   geom_boxplot()+  
   # theme(axis.text.x = element_text(angle = 45, hjust=1,size=12))+
   xlab("")+ylab("Number of same address records per case per outbreak")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2") +
   # geom_dotplot(binaxis='y', stackdir='center', dotsize=.4, fill="black")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=10))+
   scale_fill_manual(name= "Clarity", values = c("#66C2A5", "#FC8D62", "#8DA0CB"))+
   scale_color_manual(name = "Clarity", values = c("#57A58CFF", "#D67853FF",  "#7888ADFF"))

g

##ggsave(g, filename=paste0("Supplementary Figure Outbreaks K (n pos address per case).svg"), width=10, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 15 K.svg"), width=10, height = 4)
getwd()


# L	Cases: Box plot over genetically identical cases, per outbreak, by outbreak type
g<- ggplot(data=out, aes(x=type3 , y=genLinkedCommunityCases)) +
   geom_boxplot()+
   # theme(axis.text.x = element_text(angle = 45, hjust=1,size=12))+
   xlab("")+ylab("Number of genetically linked cases per outbreak")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")  + theme(legend.position = "none")+
   # geom_dotplot(binaxis='y', stackdir='center', dotsize=.4, fill="black")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=10))

g

##ggsave(g, filename=paste0("Supplementary Figure Outbreaks L (gen links).svg"), width=8, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 15 L.svg"), width=10, height = 4)


# M	Cases: Box plot over genetically identical cases, per case in outbreak, by outbreak type
out$norm_genLinkedCommunityCases <- out$genLinkedCommunityCases/out$n_casesDuesseldorf
g<- ggplot(data=out, aes(x=type3 , y=norm_genLinkedCommunityCases)) +
   geom_boxplot()+
   # theme(axis.text.x = element_text(angle = 45, hjust=1,size=12))+
   xlab("")+ylab("Number of genetically linked cases per case per outbreak")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")  + theme(legend.position = "none")+
   # geom_dotplot(binaxis='y', stackdir='center', dotsize=.4, fill="black")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=10))

g

##ggsave(g, filename=paste0("Supplementary Figure Outbreaks M (gen links per case).svg"), width=8, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 15 M.svg"), width=8, height = 4)


sum(os$n_outbreaks)
sum(os$n_outbreaksDistanceAvailable)

  
sum(as.numeric(os$n_erroneousCases))/sum(as.numeric(osOutbreak$n_sequencedCasesInoutbreaksDistanceAvailable))

sum(os$n_linkedCasesTotal)

sum(os$n_linkedCasesSeqBasedAssesed)
sum(os$n_linkedCasesSeqBasedAssesedAccepted)
sum(os$n_linkedCasesSeqBasedAssesedAccepted[os$category %in% c("School","Kindergarten/daycare" )])/sum(os$n_linkedCasesSeqBasedAssesedAccepted)

sum(os$n_genIdenticalCommunityCases)

sum(os$n_outbreaksNonClonal)



### Contact tracing
sum(nodes$Count_Kontaktpersonen_Duesseldorf)
table(edge2$Typ)

test <- unique(edge2$CaseID_Index[edge2$Typ == "SurvnetKontaktperson"])
test2 <- nodes$CaseID[!nodes$CaseID %in% test]

table(nodes$Count_Kontaktpersonen[nodes$CaseID %in% test2])

nodes$search <- NULL
table(nodes$Count_Kontaktpersonen_Duesseldorf)


#Supplementary Figure 9
##### Contacts and InfectedBy
##########
# Match by contacter persons and vaccinations and date and only use cases with genetic info to recalculate the genetic distance
## SurvnetKontaktperson
edges <- edge2

##filter nodes for only cases described only as contact person
contactpersonIDs <-  edges$CaseID_Index[edges$Typ %in%  c("SurvnetAngestecktBei", "SurvnetKontaktperson")]
contactpersonIDs2 <-  edges$CaseID_Kontaktperson[edges$Typ %in%  c("SurvnetAngestecktBei", "SurvnetKontaktperson")]
contactpersonIDs <- unique(c(contactpersonIDs, contactpersonIDs2))

# nodesCur <- nodes[nodes$CaseID %in%  contactpersonIDs,]

nodes$Index_date <- substr(nodes$Meldedatum, 1, 10)

nodes$Index_date <- as.character(lapply(nodes$Index_date, function(x){ 
   y <- strsplit(x, " ")[[1]][[1]]
   y <- gsub("\\.", "-",y)
   as.character(y)
} ))


pacman::p_load(lubridate)

nodes$Index_date <- dmy(nodes$Index_date)

## Filter cases outside of 2021
library(stringr)
nodes$indFilt <- str_detect(nodes$Index_date, "2021")
nodes <- nodes[nodes$indFilt,]
nodes$indFilt <- str_detect(nodes$Index_date, "2021-01")
nodes <- nodes[!nodes$indFilt,]

casesWithGeneticInfo <- nodes$CaseID[nodes$SeqID != ""]

anbeiGen <- edge2[edge2$Typ %in%  c("SurvnetAngestecktBei", "SurvnetKontaktperson"), ]

##calcualte distances
anbeiGen$seqIndex1 <- nodes$SeqID[match(anbeiGen$CaseID_Index, nodes$CaseID)]
anbeiGen$seqIndex2 <- nodes$SeqID[match(anbeiGen$CaseID_Kontaktperson, nodes$CaseID)]

anbeiGen$Genetic_Distance <- as.numeric(lapply(1:nrow(anbeiGen), function(x){
   
   getGeneticDistance(anbeiGen$seqIndex1[x], anbeiGen$seqIndex2[x])
   
}))

anbeiGen <- na.omit(anbeiGen)


anbeiGen$Index_date <- nodes$Index_date[match(anbeiGen$CaseID_Index, nodes$CaseID)]
anbeiGen$Contact_person_date <- nodes$Index_date[match(anbeiGen$CaseID_Kontaktperson, nodes$CaseID)]

anbeiGen$date1 <- anbeiGen$Index_date
anbeiGen$date2 <- anbeiGen$Index_date

for(i in 1:nrow(anbeiGen)){
   
   anbeiGen$date1[i] <- min(c(anbeiGen$Index_date[i], anbeiGen$Contact_person_date[i]))
   anbeiGen$date2[i] <- max(c(anbeiGen$Index_date[i], anbeiGen$Contact_person_date[i]))
   
   anbeiGen$date_pair_sort[i] <- paste( anbeiGen$date1[i], anbeiGen$date2[i])
}


## enter vac und contact person number
anbeiGen$index_vacc <- nodes$Impfung_Anzahl[match(anbeiGen$CaseID_Index, nodes$CaseID)]
anbeiGen$contact_vacc <- nodes$Impfung_Anzahl[match(anbeiGen$CaseID_Kontaktperson, nodes$CaseID)]

anbeiGen$index_nContact_persons <- nodes$Count_Kontaktpersonen[match(anbeiGen$CaseID_Index, nodes$CaseID)]
anbeiGen$contact_nContact_persons <- nodes$Count_Kontaktpersonen[match(anbeiGen$CaseID_Kontaktperson, nodes$CaseID)]


anbeiGen$nCaseIDindex <- 1
anbeiGen$nCaseIDcontact <- 1

for(i in seq_len(nrow(anbeiGen))){
   
   anbeiGen$vacc_pair[i] <-    paste(sort(c(anbeiGen$index_vacc[i], anbeiGen$contact_vacc[i])), collapse=" ")
   anbeiGen$nContact_pair[i] <- paste(sort(c(anbeiGen$index_nContact_persons[i], anbeiGen$contact_nContact_persons[i])), collapse=" ")
   
   anbeiGen$nCaseIDindex[i] <- sum(c(anbeiGen$CaseID_Index, anbeiGen$CaseID_Kontaktperson) == anbeiGen$CaseID_Index[i])
   anbeiGen$nCaseIDcontact[i] <- sum(c(anbeiGen$CaseID_Index, anbeiGen$CaseID_Kontaktperson) == anbeiGen$CaseID_Kontaktperson[i])
   
}

## Create selection table
contactpersonIDsWithGeneticInfo <- contactpersonIDs[contactpersonIDs %in% nodes2$CaseID]
nodesAnbei <- expand.grid(contactpersonIDsWithGeneticInfo, contactpersonIDsWithGeneticInfo)

## Fill in pair info
names(nodesAnbei) <- c("CaseID_Index", "CaseID_Kontaktperson")
nodesAnbei$Typ <- "pseudoSurvnetKontaktperson"
nodesAnbei$Kontaktperson_Ort <- ""
nodesAnbei$DatumsDifferenz <- 0 ## toDO
nodesAnbei$Genetic_Distance <- 0 ## toDO
nodesAnbei$Index_date <- nodes$Meldedatum[match(nodesAnbei$CaseID_Index, nodes$CaseID)]
nodesAnbei$Contact_person_date <- nodes$Meldedatum[match(nodesAnbei$CaseID_Kontaktperson, nodes$CaseID)]
nodesAnbei$date_pair <- ""  ## toDO
nodesAnbei$gen_info_indexCaseID <- nodes$SeqID[match(nodesAnbei$CaseID_Index, nodes$CaseID)]
nodesAnbei$gen_info_contactCaseID<- nodes$SeqID[match(nodesAnbei$CaseID_Kontaktperson, nodes$CaseID)]
nodesAnbei$index_vacc <- nodes$Impfung_Anzahl[match(nodesAnbei$CaseID_Index, nodes$CaseID)]
nodesAnbei$contact_vacc <- nodes$Impfung_Anzahl[match(nodesAnbei$CaseID_Kontaktperson, nodes$CaseID)]
nodesAnbei$index_nContact_persons <- nodes$Count_Kontaktpersonen[match(nodesAnbei$CaseID_Index, nodes$CaseID)]
nodesAnbei$contact_nContact_persons <- nodes$Count_Kontaktpersonen[match(nodesAnbei$CaseID_Kontaktperson, nodes$CaseID)]

nodesAnbei$Index_date <- substr(nodesAnbei$Index_date, 1, 10)
nodesAnbei$Contact_person_date <- substr(nodesAnbei$Contact_person_date, 1, 10)

nodesAnbei$Index_date <- as.character(lapply(nodesAnbei$Index_date, function(x){ 
   y <- strsplit(x, " ")[[1]][[1]]
   y <- gsub("\\.", "-",y)
   as.character(y)
} ))

nodesAnbei$Contact_person_date <- as.character(lapply(nodesAnbei$Contact_person_date, function(x){ 
   y <- strsplit(x, " ")[[1]][[1]]
   y <- gsub("\\.", "-",y)
   as.character(y)
} ))



pacman::p_load(lubridate)

nodesAnbei$Index_date <- dmy(nodesAnbei$Index_date)
nodesAnbei$Contact_person_date <- dmy(nodesAnbei$Contact_person_date)

nodesAnbei$date_pair <- paste(nodesAnbei$Index_date, nodesAnbei$Contact_person_date)

nodesAnbei$DatumsDifferenz <- nodesAnbei$Index_date - nodesAnbei$Contact_person_date
nodesAnbei$DatumsDifferenz <- as.numeric(nodesAnbei$DatumsDifferenz)

nodesAnbei <- nodesAnbei[abs(nodesAnbei$DatumsDifferenz)<15,]
nodesAnbei <- nodesAnbei[nodesAnbei$CaseID_Index != nodesAnbei$CaseID_Kontaktperson,]


## save pseudo pair table as anbei for further analysis
anbei <- nodesAnbei

library(parallel)
numCores <- detectCores()
cl <- makeCluster(10)

clusterExport(cl=cl, list("anbei", "edges", "interval"),
              envir=environment())

anbei$nCaseIDindex <- 1
anbei$nCaseIDcontact <- 1


anbei$vacc_pair <- parLapply(cl, seq_len(nrow(anbei)), function(i){
   
   paste(sort(c(anbei$index_vacc[i], anbei$contact_vacc[i])), collapse=" ")
   
})

anbei$nContact_pair <- parLapply(cl, seq_len(nrow(anbei)), function(i){
   
   paste(sort(c(anbei$index_nContact_persons[i], anbei$contact_nContact_persons[i])), collapse=" ")
   
})

anbei$nCaseIDindex <- parLapply(cl, seq_len(nrow(anbei)), function(i){
   
   sum(c(edges$CaseID_Index,edges$CaseID_Kontaktperson) == anbei$CaseID_Index[i])
   
})

anbei$nCaseIDcontact <- parLapply(cl, seq_len(nrow(anbei)), function(i){
   
   sum(c(edges$CaseID_Index,edges$CaseID_Kontaktperson) == anbei$CaseID_Kontaktperson[i])
   
})

anbei <- anbei[anbei$CaseID_Index != anbei$CaseID_Kontaktperson,]
 
anbei$date1 <- anbei$Index_date
anbei$date2 <- anbei$Index_date
anbei$meanDate <- anbei$Index_date

anbei$date1 <- parLapply(cl, seq_len(nrow(anbei)), function(i){
   
   min(c(anbei$Index_date[i], anbei$Contact_person_date[i]))
   
})

(d <- do.call("c", anbei$date1))
anbei$date1 <- d

anbei$date2 <- parLapply(cl, seq_len(nrow(anbei)), function(i){
   
   max(c(anbei$Index_date[i], anbei$Contact_person_date[i]))
   
})

(d <- do.call("c", anbei$date2))
anbei$date2 <- d

clusterExport(cl=cl, list("anbei", "edges", "interval", "as.duration"),  envir=environment())

anbei$meanDate <- parLapply(cl, seq_len(nrow(anbei)), function(i){
   
   ex = interval(anbei$date1[i], anbei$date2[i])
   meanIndexDate <- ex@start + as.duration(ex)/2
   
})

(d <- do.call("c", anbei$meanDate))
anbei$meanDate <- d



anbei$date_pair <- paste(anbei$date1, anbei$date2)
anbei$cur_dateDIff <-0
anbei$searchPair <- paste(anbei$CaseID_Index, anbei$CaseID_Kontaktperson)


## overwrite date1 2

anbeiGen$date1 <- as.character(lapply(anbeiGen$date_pair, function(x){
   
   strsplit(x, " ")[[1]][[1]]
   
}))

anbeiGen$date2 <- as.character(lapply(anbeiGen$date_pair, function(x){
   
   strsplit(x, " ")[[1]][[2]]
   
}))

anbeiGen$date_pair_sort <- anbeiGen$date_pair


anbei$caseIDcountIndex <- 0
anbei$caseIDcountKontakt <- 0

edgesTbl <- table(edge2$CaseID_Index)
edgesTblcontact <- table(edge2$CaseID_Kontaktperson)


clusterExport(cl=cl, list("anbei", "edges", "interval", "as.duration", "edgesTbl", "edgesTblcontact"),  envir=environment())

anbei$caseIDcountIndex <- parLapply(cl, seq_len(nrow(anbei)), function(i){
   
   edgesTbl[as.character(anbei$CaseID_Index[i])]
   
})

anbei$caseIDcountKontakt <- parLapply(cl, seq_len(nrow(anbei)), function(i){
   
   edgesTblcontact[as.character(anbei$CaseID_Index[i])]
   
})

anbei$caseIDcountIndex  <- as.numeric(anbei$caseIDcountIndex )
anbei$caseIDcountKontakt  <- as.numeric(anbei$caseIDcountKontakt )

anbei$caseIDcountIndex[is.na( anbei$caseIDcountIndex)] <- 0
anbei$caseIDcountKontakt[is.na( anbei$caseIDcountKontakt)] <- 0


anbei$nContact <- as.character(anbei$nContact)
anbei$nContact_mean <- as.numeric(parLapply(cl, anbei$nContact, function(i){
   
   
   y <- strsplit(i, " ")[[1]]
   y <- mean(as.numeric(y))
   y
   
}))



anbei$ID <- 1:nrow(anbei)


pacman::p_load(lubridate)


anbei$searchID <- as.character(parLapply(cl, seq_len(nrow(anbei)), function(i){
   
   cs <- c(anbei$CaseID_Index[i], anbei$CaseID_Kontaktperson[i])
   cs <- sort(cs)
   paste(cs, collapse = " ")
   
}))


anbeiGen$searchID <- as.character(lapply(seq_len(nrow(anbeiGen)),function(x){
   
   cs <- c(anbeiGen$CaseID_Index[x], anbeiGen$CaseID_Kontaktperson[x])
   cs <- sort(cs)
   paste(cs, collapse = " ")
   
}))

anbei <- anbei[!anbei$searchID %in% anbeiGen$searchID,]

anbei$CaseID_Index <- as.character( anbei$CaseID_Index)
anbei$CaseID_Kontaktperson <- as.character( anbei$CaseID_Kontaktperson)

anbei$searchPair <- anbei$searchID

anbeiGen$index1 <- as.character(lapply(1:nrow(anbeiGen), function(x){
   
   i1 <- anbeiGen$CaseID_Index[x]
   i2 <- anbeiGen$CaseID_Kontaktperson[x]
   
   ids <- sort(c(i1, i2))
   ids[1]
   
}))

anbeiGen$index2 <- as.character(lapply(1:nrow(anbeiGen), function(x){
   
   i1 <- anbeiGen$CaseID_Index[x]
   i2 <- anbeiGen$CaseID_Kontaktperson[x]
   
   ids <- sort(c(i1, i2))
   ids[2]
   
}))

anbeiGen$searchPair <- paste(anbeiGen$index1, anbeiGen$index2)


## Percalculate contact info

contactList <- list()

allAnbeiIDs <- unique(c(anbei$CaseID_Index, anbei$CaseID_Kontaktperson))

for(i in allAnbeiIDs){
   
   randomPairContactsIndex <- edge2[(edge2$CaseID_Index == i) |
                                       (edge2$CaseID_Kontaktperson ==  i),]
   randomPairContactsIndex <- c(randomPairContactsIndex$CaseID_Index,  randomPairContactsIndex$CaseID_Kontaktperson)
   contactList[[i]] <- unique(randomPairContactsIndex)
}



##check if some show same contacts 


clusterExport(cl=cl, list("anbei", "edges", "interval", "as.duration", "edgesTbl", "edgesTblcontact", "contactList"),  envir=environment())


anbei$OverlappingContacts <- as.numeric(parLapply(cl, seq_len(nrow(anbei)), function(i){
   
   contactList1 <- contactList[[anbei$CaseID_Index[[i]]]]
   contactList2 <- contactList[[anbei$CaseID_Kontaktperson[[i]]]]
   
   sum(contactList1 %in% contactList2)
   
}))


#save.image("AllContactsTestDataAllContact_bevorSameContactsCheck.RData")
 
anbei <- anbei[anbei$OverlappingContacts == 0,]

anbei <- anbei[!anbei$searchPair %in% anbeiGen$searchPair,]



options(warn=2)

listpairs <- list()
missing <-  0
i <- 1

for(i in seq_len(nrow(anbeiGen))){
   
   vacc <-    anbeiGen$vacc_pair[i]
   nContact <- anbeiGen$nContact_pair[i]
   
   caseIDcountIndex <- sum(edge2$CaseID_Index == anbeiGen$CaseID_Index[i])
   caseIDcountKontakt <- sum(edge2$CaseID_Kontaktperson == anbeiGen$CaseID_Index[i])
   
   ## Get exact match
   exact <- anbei[anbei$vacc_pair == vacc & anbei$nContact == nContact,]
   
   ## if exact match found continue, if not search for next best solution
   if(nrow(exact)==0){
      
      exact <- anbei[anbei$vacc_pair == vacc,]
      
   }
      
   d1 <- min(c(anbeiGen$date1[i], anbeiGen$date2[i]))
   d2 <- max(c(anbeiGen$date1[i], anbeiGen$date2[i]))
   
   aGdD <- anbeiGen$DatumsDifferenz[i]
   
   ##get mean date
   ex = interval(as.Date(d1), as.Date(d2))
   meanIndexDate <- ex@start + as.duration(ex)/2
   meanIndexDate <- substr(meanIndexDate, 1, 10)
   
   exact$cur_dateDIff <- 0
   exact$cur_dateDIff <- difftime(meanIndexDate, exact$meanDate, units = "days")
   exact$cur_dateDIff <- as.numeric(round(exact$cur_dateDIff,0))
   
   exact$index_date_diff <- 0
   exact$index_date_diff <- difftime(d1, exact$date1, units = "days")
   exact$index_date_diff <- as.numeric(round(exact$index_date_diff,0))
   
   exact$DatumsDifferenz_diff <- exact$DatumsDifferenz - unique(abs(aGdD))
   
   exact$caseIDcountIndex <- exact$caseIDcountIndex-caseIDcountIndex
   exact$caseIDcountKontakt <- exact$caseIDcountKontakt-caseIDcountKontakt
   
   ##filter mean contact person
   nContact <- as.numeric(str_split(nContact, " ")[[1]])
   nContact <- sort(nContact)
   
   exact$nContact_mean_diff <- as.numeric(lapply(exact$nContact, function(x){
      
      exCont <- as.numeric(str_split(x, " ")[[1]])
      exCont <- sort(exCont)
      
      exCont <- abs(exCont - nContact)
      exCont <- sum(exCont)
      exCont
   }))
   
   #exact$nContact_mean_diff <- exact$nContact_mean - mean(as.numeric(strsplit(nContact, " ")[[1]]))
   
   #exact <- exact[order(abs(exact$index_date_diff), abs(exact$cur_dateDIff), abs(exact$DatumsDifferenz_diff), abs(exact$nContact_mean_diff ), abs(exact$caseIDcountIndex), abs(  exact$caseIDcountKontakt)),]
   exact <- exact[order(abs(exact$index_date_diff), abs(exact$DatumsDifferenz_diff), abs(exact$cur_dateDIff), abs(exact$nContact_mean_diff ), abs(exact$caseIDcountIndex), abs(  exact$caseIDcountKontakt)),]
   
   exact$DatumsDifferenz_diff <- NULL
   exact$caseIDcountIndex <- NULL
   exact$caseIDcountKontakt <- NULL
   exact$index_date_diff <- NULL
   
   exact <- exact[1,]
   listpairs[[i]] <- exact
   
   anbei$CaseID_Index[anbei$ID == exact$ID] <- NA
   
   
   anbei <- na.omit(anbei)   
      
   
}

options(warn=1)

library(data.table)
DFpairs <- rbindlist(listpairs)
DFpairs <- na.omit(DFpairs)


which(DFpairs$searchPair %in% edge2$indexSearch)

#save.image("AllContactsTestDataAllContact2.RData")
#load("AllContactsTestDataAllContact2.RData")

##calcualte DFpairs distances
DFpairs$seqIndex1 <- nodes$SeqID[match(DFpairs$CaseID_Index, nodes$CaseID)]
DFpairs$seqIndex2 <- nodes$SeqID[match(DFpairs$CaseID_Kontaktperson, nodes$CaseID)]

DFpairs$Genetic_Distance <- as.numeric(lapply(1:nrow(DFpairs), function(x){
   
   getGeneticDistance(DFpairs$seqIndex1[x], DFpairs$seqIndex2[x])
   
}))


DFpairs$foundForwardContactTracingLink <- 0
edge2$indexSearch2 <- paste(edge2$index1, edge2$index2)
DFpairs$foundForwardContactTracingLink[DFpairs$searchPair %in% edge2$indexSearch2] <- 1




DFpairs$c <- 1
anbeiGen$c <- 1

DFpairs$DatumsDifferenzABS <- abs(DFpairs$DatumsDifferenz)
anbeiGen$DatumsDifferenzABS <- abs(anbeiGen$DatumsDifferenz)

DFpairs_datediffcount <- tapply(DFpairs$c ,DFpairs$DatumsDifferenzABS, sum)
anbeiGen_datediffcount <- tapply(anbeiGen$c ,anbeiGen$DatumsDifferenzABS, sum)

res <- data.frame(Date_diff= c(names(DFpairs_datediffcount), names(anbeiGen_datediffcount)), 
                  count= c(DFpairs_datediffcount,anbeiGen_datediffcount ), 
                  type=c(rep("Random case pair", length(DFpairs_datediffcount)),
                         rep("Contact tracing case pair", length(anbeiGen_datediffcount))
                  ))

res$Date_diff <- factor(res$Date_diff, levels=as.character(unique(sort(as.numeric(res$Date_diff)))))

library(ggplot2)
g<- ggplot(data=res, aes(x=Date_diff, y=count, fill=type)) +
   geom_bar(stat="identity",position = position_dodge(preserve = "single"))+ 
   #theme(axis.text.x = element_text(angle = 45, hjust=1))+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=15))+
   theme(legend.title=element_blank())+xlab("Date difference in days")+
   ylab("Number of case pairs")

g

#setwd("~/Desktop/HomeOffice/NUM")
##ggsave(g, file= "Supplementary Figure DistancesScrambledLinks (allContact date diff).svg", width = 7, height=4)
#ggsave(g, file= "Supplementary Figure 9 A.svg", width = 7, height=4)




### count vaccination status
DFpairs$CaseID_Index <- as.character(DFpairs$CaseID_Index)
DFpairs$CaseID_Kontaktperson <- as.character(DFpairs$CaseID_Kontaktperson)

caseIDs_DFpairs <- unique(c(DFpairs$CaseID_Index, DFpairs$CaseID_Kontaktperson))
caseIDs_anbeiGen <- unique(c(anbeiGen$CaseID_Index, anbeiGen$CaseID_Kontaktperson))

caseIDs_DFpairs_n_vacc <- nodes2$Impfung_Anzahl[nodes2$CaseID %in% caseIDs_DFpairs]
caseIDs_anbeiGen_n_vacc <- nodes2$Impfung_Anzahl[nodes2$CaseID %in% caseIDs_anbeiGen]

caseIDs_DFpairs_n_vacc_count <- table(caseIDs_DFpairs_n_vacc)
caseIDs_anbeiGen_n_vacc_count <- table(caseIDs_anbeiGen_n_vacc)

res <- data.frame(n_vaccinations= c(names(caseIDs_DFpairs_n_vacc_count), names(caseIDs_anbeiGen_n_vacc_count)), 
                  count= c(caseIDs_DFpairs_n_vacc_count, caseIDs_anbeiGen_n_vacc_count ), 
                  type=c(rep("Random case pair", length(caseIDs_DFpairs_n_vacc_count)),
                         rep("Contact tracing case pair", length(caseIDs_anbeiGen_n_vacc_count))
                  ))

res$n_vaccinations[res$n_vaccinations == "-nicht erhoben-"] <- "Not\navailable"

library(ggplot2)
g<- ggplot(data=res, aes(x=n_vaccinations, y=count, fill=type)) +
   geom_bar(stat="identity",position = "dodge")+ 
   #theme(axis.text.x = element_text(angle = 45, hjust=1))+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=15))+
   theme(legend.title=element_blank())+xlab("Number of prior vaccinations")+
   ylab("Number of cases")

g

##ggsave(g, file= "Supplementary Figure DistancesScrambledLinks (allContact vacc).svg", width = 6, height=4)
#ggsave(g, file= "Supplementary Figure 9 C.svg", width = 6, height=4)






### count number of contacts
 
caseIDs_DFpairs_n_vacc <- nodes2$Count_Kontaktpersonen_Duesseldorf[nodes2$CaseID %in% caseIDs_DFpairs]
caseIDs_anbeiGen_n_vacc <- nodes2$Count_Kontaktpersonen_Duesseldorf[nodes2$CaseID %in% caseIDs_anbeiGen]

caseIDs_DFpairs_n_vacc[caseIDs_DFpairs_n_vacc >=10] <- ">=10"
caseIDs_anbeiGen_n_vacc[caseIDs_anbeiGen_n_vacc >=10] <- ">=10"

caseIDs_DFpairs_n_vacc_count <- table(caseIDs_DFpairs_n_vacc)
caseIDs_anbeiGen_n_vacc_count <- table(caseIDs_anbeiGen_n_vacc)


res <- data.frame(n_contact_persons= c(names(caseIDs_DFpairs_n_vacc_count), names(caseIDs_anbeiGen_n_vacc_count)), 
                  count= c(caseIDs_DFpairs_n_vacc_count, caseIDs_anbeiGen_n_vacc_count ), 
                  type=c(rep("Random case pair", length(caseIDs_DFpairs_n_vacc_count)),
                         rep("Contact tracing case pair", length(caseIDs_anbeiGen_n_vacc_count))
                  ))

res$n_contact_persons <- factor(res$n_contact_persons, levels= c(as.character(0:9), ">=10"))


library(ggplot2)
g<- ggplot(data=res, aes(x=n_contact_persons, y=count, fill=type)) +
   geom_bar(stat="identity",position = "dodge")+ 
   #theme(axis.text.x = element_text(angle = 45, hjust=1))+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=15))+
   theme(legend.title=element_blank())+xlab("Number of contacts")+
   ylab("Number of cases")


g

##ggsave(g, file= "Supplementary Figure DistancesScrambledLinks (allContact n conctacts).svg", width =7, height=4)
#ggsave(g, file= "Supplementary Figure 9 B.svg", width = 7, height=4)


getwd()








### genetic distance
caseIDs_DFpairs_n_vacc  <- abs(DFpairs$Genetic_Distance)
caseIDs_anbeiGen_n_vacc <- abs(anbeiGen$Genetic_Distance)

caseIDs_DFpairs_n_vacc[caseIDs_DFpairs_n_vacc >=40] <- ">=40"
caseIDs_anbeiGen_n_vacc[caseIDs_anbeiGen_n_vacc >=40] <- ">=40"


caseIDs_DFpairs_n_vacc_count <- table(caseIDs_DFpairs_n_vacc)
caseIDs_anbeiGen_n_vacc_count <- table(caseIDs_anbeiGen_n_vacc)




res <- data.frame(n_contact_persons= c(names(caseIDs_DFpairs_n_vacc_count), names(caseIDs_anbeiGen_n_vacc_count)), 
                  count= c(caseIDs_DFpairs_n_vacc_count, caseIDs_anbeiGen_n_vacc_count ), 
                  type=c(rep("Random case pair", length(caseIDs_DFpairs_n_vacc_count)),
                         rep("Contact tracing case pair", length(caseIDs_anbeiGen_n_vacc_count))
                  ))

res$n_contact_persons <- factor(res$n_contact_persons, levels= c(as.character(0:39), ">=40"))


library(ggplot2)
g<- ggplot(data=res, aes(x=n_contact_persons, y=count, fill=type)) +
   geom_bar(stat="identity",position = position_dodge2(preserve='single'))+ 
   #theme(axis.text.x = element_text(angle = 45, hjust=1))+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=15))+
   theme(legend.title=element_blank())+xlab("Genetic distance")+
   ylab("Number of case pairs")


g


##ggsave(g, file= "Supplementary Figure DistancesScrambledLinks (allContact distances).svg", width = 16, height=4)
#ggsave(g, file= "Supplementary Figure 9 D.svg", width = 16, height=4)





## Delete unneeded columns
saveColumns <- c("CaseID_Index",	"CaseID_Kontaktperson",	"Typ",	"DatumsDifferenz",
                 "Genetic_Distance",	"Index_date",	"Contact_person_date",
                 "gen_info_indexCaseID",	"gen_info_contactCaseID",	"index_vacc",
                 "contact_vacc",	"index_nContact_persons",	"contact_nContact_persons")
DFpairs <- as.data.frame(DFpairs)

DFpairs <- DFpairs[, c(names(DFpairs) %in% saveColumns)]
anbeiGen <- anbeiGen[, names(anbeiGen) %in% saveColumns]

## save
save(DFpairs, anbeiGen, file="Matched pairs allContact Mock")


mean(DFpairs$Genetic_Distance)

sum(DFpairs$Genetic_Distance <2)/nrow(DFpairs)




### calendar week of cases
nodes$month <- strftime(nodes$Meldedatum2, format = "%V")
nodes$c <- 1

caseIDs_DFpairs_n_vacc <- nodes$month[nodes$CaseID %in% caseIDs_DFpairs]
caseIDs_anbeiGen_n_vacc <- nodes$month[nodes$CaseID %in% caseIDs_anbeiGen]

caseIDs_DFpairs_n_vaccDT <- data.frame(table(caseIDs_DFpairs_n_vacc))
caseIDs_anbeiGen_n_vaccDT <- data.frame(table(caseIDs_anbeiGen_n_vacc))

names(caseIDs_DFpairs_n_vaccDT) <- c("month", "freq")
names(caseIDs_anbeiGen_n_vaccDT) <- c("month", "freq")

caseIDs_DFpairs_n_vaccDT$type <- "Random case pair"
caseIDs_anbeiGen_n_vaccDT$type <- "Contact tracing case pair"

geneticsPerMonth <- rbind(caseIDs_DFpairs_n_vaccDT, caseIDs_anbeiGen_n_vaccDT)
 
geneticsPerMonth$month  <- factor(geneticsPerMonth$month, levels=c("01","02","03","04",
                                                                   "05", "06", "07", "08",
                                                                   "09", "10", as.character(11:52)))
 

library(ggplot2)
g<- ggplot(data=geneticsPerMonth, aes(x=month, y=freq, fill=type)) +
   geom_bar(stat="identity",position = position_dodge2(preserve='single'))+ 
   #theme(axis.text.x = element_text(angle = 45, hjust=1))+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=15))+
   theme(legend.title=element_blank())+xlab("Calendar week 2021")+
   ylab("Number of cases")


g


##ggsave(g, file= "Supplementary Figure DistancesScrambledLinks (allContact distances).svg", width = 16, height=4)
#ggsave(g, file= "Supplementary Figure 9 E.svg", width = 16, height=4)






### Contact tracing Figure
## Figure 3

contacts  <- edge2
contacts <- contacts[contacts$indexSearch %in% edgesGraph$indexSearch,]
contacts <- contacts[contacts$DatumsDifferenz <= 14 ,]

anbei <- contacts

anbei$index1_seqID <- nodes$SeqID[match(anbei$CaseID_Index, nodes$CaseID)]
anbei$index2_seqID <- nodes$SeqID[match(anbei$CaseID_Kontaktperson, nodes$CaseID)]

anbei$sequencedCase <- 0
anbei$sequencedCase[anbei$index1_seqID != "" | anbei$index2_seqID != ""] <- 0
anbei$sequencedCase[anbei$index1_seqID != "" & anbei$index2_seqID != ""] <- 2

anbei$CaseID_Index_start <- NULL
anbei$CaseID_Kontaktperson_start <- NULL

anbei2 <- anbei[,which(names(anbei) %in% c("indexSearch", "Typ","sequencedCase"))]
anbei2 <- unique(anbei2)

res <- data.frame(table(anbei2$Typ ,anbei2$sequencedCase))

res <- res[res$Var1 %in% c("SelbeAdresse","SelbeAdresseNachname", "SurvnetAngestecktBei" ,"SurvnetKontaktperson"),]

res$groupCount <- 0
res$groupCount[res$Var1 == "SelbeAdresse"] <- sum(res$Freq[res$Var1 == "SelbeAdresse"] )
res$groupCount[res$Var1 == "SelbeAdresseNachname"] <- sum(res$Freq[res$Var1 == "SelbeAdresseNachname"] )
res$groupCount[res$Var1 == "SurvnetAngestecktBei"] <- sum(res$Freq[res$Var1 == "SurvnetAngestecktBei"] )
res$groupCount[res$Var1 == "SurvnetKontaktperson"] <- sum(res$Freq[res$Var1 == "SurvnetKontaktperson"] )


res$percentage <- res$Freq/res$groupCount*100

res$Var2 <- as.character(res$Var2)
res$Var2[res$Var2 == "0"] <- "No distance available"
res$Var2[res$Var2 == "2"] <- "Distance available"

res$category <- factor(res$Var2, levels= c("No distance available","Distance available"))
res$percentage <- round(res$percentage,1)
res$percentage_lable <- paste0(res$percentage, "%")

library(ggplot2)
library(ggbreak)
library(patchwork)


res$Var1_1 <- as.character(res$Var1)

res$Var1_1[res$Var1_1 == "SelbeAdresse"] <- "Same address"
res$Var1_1[res$Var1_1 == "SurvnetAngestecktBei"] <- "Backward\ncontact tracing"
res$Var1_1[res$Var1_1 == "SurvnetKontaktperson"] <- "Forward\ncontact tracing"

res <- res[res$Var1_1 != "SelbeAdresseNachname",]

res$Var1_1 <- factor(res$Var1_1, levels=c("Forward\ncontact tracing","Backward\ncontact tracing","Same address"))

res$percentage2 <- res$percentage/100

library(RColorBrewer)
g<- ggplot(data=res, aes(x=Var1_1, y=Freq, fill=category)) +
   geom_bar(stat="identity",position = "dodge")+
   # theme(axis.text.x = element_text(angle = 45, hjust=1))+
   xlab("")+ylab("Number of case pairs")+
   scale_fill_brewer(palette="Set2")+
   geom_text(aes(label = percentage_lable),position=position_dodge(width=0.9), vjust=-0.2, size=5)


g <- g +
   guides(fill=guide_legend(title=""))

g <- g+ scale_fill_manual(name = "",
                          values = c("#66C2A5", "#FC8D62"))+
   theme(text = element_text(size=17))

g <- g+theme(
   # text = element_text(family = "Arial"),
   axis.text.x = element_text( size = 15),#,angle = 45, hjust=1),
   axis.text.y = element_text(size = 15),
   axis.title = element_text(size = 18),
   # legend.text = element_text(family = "Arial", size = 15),
   plot.background = element_rect(fill = "white", color = NA),
   panel.background = element_rect(fill = "white", color = NA),
   panel.grid.major = element_line(color = "grey80"),
   panel.grid.minor = element_blank(),
   axis.ticks = element_blank() #,
   #plot.margin = margin(t = 10, r = 30, b = 10, l = 40),
)

g


##ggsave(g, filename=paste0("Contact Tracing A (n_sequenced).svg"), width=8.2, height = 5)
#ggsave(g, filename=paste0("Figure 3 B.svg"), width=8.2, height = 5)
getwd()




gen <- anbei2[anbei2$sequencedCase == "2",]

gen$index1 <- as.character(lapply(gen$indexSearch, function(x){ strsplit(x, " ")[[1]][[1]]}))
gen$index2 <- as.character(lapply(gen$indexSearch, function(x){ strsplit(x, " ")[[1]][[2]]}))

gen$CaseID_Index_seqID <- nodes$SeqID[match(gen$index1, nodes$CaseID)]
gen$CaseID_Kontaktperson_seqID <- nodes$SeqID[match(gen$index2, nodes$CaseID)]

#gen$Genetic_Distance <- NULL

genGen <- gen[gen$CaseID_Index_seqID != "" & gen$CaseID_Kontaktperson_seqID != "",]
genGen <- na.omit(genGen)

genGen$Genetic_Distance2 <- lapply(seq_len(nrow(genGen)), function(x){
   
   c1 <- genGen$CaseID_Index_seqID[x]
   c2 <- genGen$CaseID_Kontaktperson_seqID[x]
   
   getGeneticDistance(c1, c2)
   
})

genGen$Genetic_Distance2 <- as.numeric(genGen$Genetic_Distance2)

genGen_suplContactTracingInterValDist <- genGen

test <- table(genGen$Genetic_Distance2, genGen$Typ)
test <- data.frame(test)

test <- test[test$Var2  != "SelbeAdresseNachname",]

test$Var1 <- as.numeric(as.character(test$Var1))
test$Var2 <-  as.character(test$Var2) 

test$Var1 <- as.numeric(test$Var1)

test$Freq[test$Var1 == 5 & test$Var2 == "SelbeAdresse"] <- sum(test$Freq[test$Var1 >= 5 & test$Var2 == "SelbeAdresse"])
test$Freq[test$Var1 == 5 & test$Var2 == "SurvnetAngestecktBei"] <-  sum(test$Freq[test$Var1 >= 5 & test$Var2 == "SurvnetAngestecktBei"])
test$Freq[test$Var1 == 5 & test$Var2 == "SurvnetKontaktperson"] <-  sum(test$Freq[test$Var1 >= 5 & test$Var2 == "SurvnetKontaktperson"])
test$Freq[test$Var1 == 5 & test$Var2 == "Outbreak"] <-  sum(test$Freq[test$Var1 >= 5 & test$Var2 == "Outbreak"])

test <- test[test$Var1 <= 5,]

test$percent <- 0
test$percent[test$Var2 == "SelbeAdresse"] <- round(test$Freq[test$Var2 == "SelbeAdresse"] /sum(test$Freq[test$Var2 == "SelbeAdresse"])*100,1)
test$percent[test$Var2 == "SurvnetAngestecktBei"] <-  round(test$Freq[test$Var2 == "SurvnetAngestecktBei"] /sum(test$Freq[test$Var2 == "SurvnetAngestecktBei"])*100,1)
test$percent[test$Var2 == "SurvnetKontaktperson"] <-  round(test$Freq[test$Var2 == "SurvnetKontaktperson"] /sum(test$Freq[test$Var2 == "SurvnetKontaktperson"])*100,1)
test$percent[test$Var2 == "Outbreak"] <-  round(test$Freq[test$Var2 == "Outbreak"] /sum(test$Freq[test$Var2 == "Outbreak"])*100,1)

test <- test[test$Var2 != "True_SelbeAdresse",]
test <- test[test$Var2 != "Outbreak",]

test$Var2[test$Var2 == "SelbeAdresse"] <- "Same address"
test$Var2[test$Var2 == "SurvnetAngestecktBei"] <- "Backward\ncontact tracing"
test$Var2[test$Var2 == "SurvnetKontaktperson"] <- "Forward\ncontact tracing"

test <- test[test$Var2 != "SelbeAdresseNachname",]

test$Var2 <- factor(test$Var2, levels=c("Forward\ncontact tracing","Backward\ncontact tracing","Same address"))

test$Var1[test$Var1 == 5] <- ">= 5"
test$Var1 <- factor(test$Var1, levels=c("0","1","2","3","4",">= 5"))
test$percentsave  <- test$percent

test$percent <- paste0(test$percent, "%")

library(ggplot2)
g <- ggplot(test, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=5)+
   xlab("Genetic distance")+ylab("Number of case pairs")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+
   theme(text = element_text(size=17))

g <- g + facet_grid(. ~ test$Var2, scales="free", shrink=T)+theme(plot.title = element_text(hjust = 0.5))

g
g <- g+theme(
   # text = element_text(family = "Arial"),
   axis.text.x = element_text( size = 15),#,angle = 45, hjust=1),
   axis.text.y = element_text(size = 15),
   axis.title = element_text(size = 18),
   # legend.text = element_text(family = "Arial", size = 15),
   plot.background = element_rect(fill = "white", color = NA),
   panel.background = element_rect(fill = "white", color = NA),
   panel.grid.major = element_line(color = "grey80"),
   panel.grid.minor = element_blank(),
   axis.ticks = element_blank() #,
   #plot.margin = margin(t = 10, r = 30, b = 10, l = 40),
)

g

##foward new: 80.5 old 80.2
##backward new: 82.4 old 82.4
##address new: 79.5 old 79.2

genGen$Genetic_Distance2_OLD <- lapply(seq_len(nrow(genGen)), function(x){
   
   c1 <- genGen$CaseID_Index_seqID[x]
   c2 <- genGen$CaseID_Kontaktperson_seqID[x]
   
   getGeneticDistance(c1, c2)
   
})

genGen$Genetic_Distance2_OLD <- as.numeric(genGen$Genetic_Distance2_OLD)

genGen$geneticDifference <- genGen$Genetic_Distance2 - genGen$Genetic_Distance2_OLD 
table (genGen$geneticDifference)

#  -4   -2   -1    0  (4% with diff != 0)
#   3   28  195 5804 

#  -4   -2   -1    0  (4% with diff != 0)
#   3   28  195 5804 

table (genGen$geneticDifference[genGen$Genetic_Distance2 == 1 & genGen$Genetic_Distance2_OLD == 2])



##ggsave(g, filename=paste0("Contact Tracing B (gen distance).svg"), width=11, height = 5)
#ggsave(g, filename=paste0("Figure 3 C.svg"), width=11, height = 5)

getwd()

## calculate rejected and accepted per category
test2 <- test[test$Var1 %in% c("0", "1"),]

tapply(test2$percentsave, test2$Var2, sum)

# Contact person    Infected by   Same address 
# 80.5               82.4           79.5


# Supplementary Figure ContactTracingGeneticDistances
## Same as Figure B above without summarizing >=5 genetic distances
gen <- anbei2[anbei2$sequencedCase == "2",]
gen$index1 <- as.character(lapply(gen$indexSearch, function(x){ strsplit(x, " ")[[1]][[1]]}))
gen$index2 <- as.character(lapply(gen$indexSearch, function(x){ strsplit(x, " ")[[1]][[2]]}))


gen$CaseID_Index_seqID <- nodes$SeqID[match(gen$index1, nodes$CaseID)]
gen$CaseID_Kontaktperson_seqID <- nodes$SeqID[match(gen$index2, nodes$CaseID)]

#gen$Genetic_Distance <- NULL

genGen <- gen[gen$CaseID_Index_seqID != "" & gen$CaseID_Kontaktperson_seqID != "",]
genGen <- na.omit(genGen)

genGen$Genetic_Distance2 <- lapply(seq_len(nrow(genGen)), function(x){
   
   c1 <- genGen$CaseID_Index_seqID[x]
   c2 <- genGen$CaseID_Kontaktperson_seqID[x]
   
   getGeneticDistance(c1, c2)
   
})

genGen$Genetic_Distance2 <- as.numeric(genGen$Genetic_Distance2)

test <- table(genGen$Genetic_Distance2, genGen$Typ)
test <- data.frame(test)

test <- test[test$Var2  != "SelbeAdresseNachname",]

test$Var1 <- as.numeric(as.character(test$Var1))
test$Var2 <-  as.character(test$Var2) 

test$percent <- 0
test$percent[test$Var2 == "SelbeAdresse"] <- round(test$Freq[test$Var2 == "SelbeAdresse"] /sum(test$Freq[test$Var2 == "SelbeAdresse"])*100,1)
test$percent[test$Var2 == "SurvnetAngestecktBei"] <-  round(test$Freq[test$Var2 == "SurvnetAngestecktBei"] /sum(test$Freq[test$Var2 == "SurvnetAngestecktBei"])*100,1)
test$percent[test$Var2 == "SurvnetKontaktperson"] <-  round(test$Freq[test$Var2 == "SurvnetKontaktperson"] /sum(test$Freq[test$Var2 == "SurvnetKontaktperson"])*100,1)
test$percent[test$Var2 == "Outbreak"] <-  round(test$Freq[test$Var2 == "Outbreak"] /sum(test$Freq[test$Var2 == "Outbreak"])*100,1)


test <- test[test$Var2 != "True_SelbeAdresse",]
test <- test[test$Var2 != "Outbreak",]

test$Var2[test$Var2 == "SelbeAdresse"] <- "Same address"
test$Var2[test$Var2 == "SurvnetAngestecktBei"] <- "Backward\ncontact tracing"
test$Var2[test$Var2 == "SurvnetKontaktperson"] <- "Forward\ncontact tracing"

test <- test[test$Var2 != "SelbeAdresseNachname",]

test$Var2 <- factor(test$Var2, levels=c("Forward\ncontact tracing","Backward\ncontact tracing","Same address"))


#test$Var1[test$Var1 == 5] <- ">= 5"
test$Var1 <- factor(test$Var1, levels=unique(test$Var1))
#test$Var1 <- as.character(test$Var1 )
test$percentsave  <- test$percent

test$percent <- paste0(test$percent, "%")

library(ggplot2)
g <- ggplot(test[test$Var2 %in% c("Forward\ncontact tracing","Backward\ncontact tracing"),], aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   # geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Genetic distance")+ylab("Number of case-case connections")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+
   theme(text = element_text(size=15))+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g <- g + facet_grid(. ~ test$Var2[test$Var2 %in% c("Forward\ncontact tracing","Backward\ncontact tracing")], scales="free", shrink=T)+theme(plot.title = element_text(hjust = 0.5))


g2 <- ggplot(test[test$Var2 %in% c('Same address'),], aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   # geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Genetic distance")+ylab("Number of case-case connections")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+
   theme(text = element_text(size=15))+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g2 <- g2 + facet_grid(. ~ test$Var2[test$Var2 %in% c('Same address')], scales="free", shrink=T)+theme(plot.title = element_text(hjust = 0.5))



library(gridExtra)


lay <- rbind(c(0,0,0,0),
             c(1,1,NA,NA))
g3 <- grid.arrange(g, g2, layout_matrix = lay, nrow=2)

getwd()


##ggsave(g3, filename="Supplementary Figure ContactTracingGeneticDistances.svg", width=25, height = 8)
#ggsave(g3, filename="Supplementary Figure 6.svg", width=25, height = 8)




# C) Infection contexts for contact tracing and infection source by "accepted" / rejected"


#load("firstWDsave.RData")

## Coco Infected By pairs

##mock infectbyContext
mock_InfBy <- edge[edge$Typ == "SurvnetAngestecktBei",]

mock_InfBy$Infektionskontext <- c("privates Treffen","privates Treffen", "häusliches Umfeld",
                                  "häusliches Umfeld" , "häusliches Umfeld" )

mock_InfBy$CaseID_Index1 <- mock_InfBy$CaseID_Kontaktperson
mock_InfBy$CaseID_Index2 <- mock_InfBy$CaseID_Index
 
## Load infectedBy contact pairs, with or without genetic information, where context was added manually
# library(readxl)
# anbei_with <- read_excel("AngestecktBei pairs (1).xlsx", 
#                          sheet = "pairs with genetic info")# 
# anbei_without <- read_excel("AngestecktBei pairs (1).xlsx", 
#                             sheet = "pairs without genetic info")

anbei_with <- mock_InfBy[c(1:3),]
anbei_without <- mock_InfBy[c(4:5),]

anbei_with$type <- "with distance"
anbei_without$type <- "without distance"

 
#contacts <- rbind(anbei_with, anbei_without)
contacts <-anbei_with

contacts$CaseID_Index1 <- as.character(gsub(" ", "", contacts$CaseID_Index1))
contacts$CaseID_Index2 <- as.character(gsub(" ", "", contacts$CaseID_Index2))
contacts$CaseID_Index1 <- as.character(gsub("[\r\n]", "", contacts$CaseID_Index1))
contacts$CaseID_Index2 <- as.character(gsub("[\r\n]", "", contacts$CaseID_Index2))


contacts <- data.frame(contacts)
#contacts$contactsetic_Distance <- NULL

contacts$CaseID <- ""
contacts$CaseID_Kontaktperson <- ""

edgesGraphInfectedBy <- edgesGraph[edgesGraph$Typ == "SurvnetAngestecktBei",]
edgesGraphInfectedBy <- unique(edgesGraphInfectedBy)
#edgesGraphInfectedBy$caseIDs <- paste0(edgesGraphInfectedBy$CaseID_Index, edgesGraphInfectedBy$CaseID_Kontaktperson)


contacts$indexSearch <- as.character(lapply(1:nrow(contacts), function(x){
   
   i1 <- sort(c(contacts$CaseID_Index2[x], contacts$CaseID_Index1[x]))[1]
   i2 <- sort(c(contacts$CaseID_Index2[x], contacts$CaseID_Index1[x]))[2]
   
   paste(i1,i2)
   
}))

contacts$indexSearch <- paste(contacts$indexSearch, "SurvnetAngestecktBei")

contacts <- contacts[contacts$indexSearch %in% edgesGraphInfectedBy$indexSearch,]
contacts <- contacts[contacts$DatumsDifferenz <= 14 ,]

#contacts <- contacts[abs(contacts$DatumsDifferenz) <= 14,]
anbei <- contacts

anbei$index1_seqID <- nodes$SeqID[match(anbei$CaseID_Index1, nodes$CaseID)]
anbei$index2_seqID <- nodes$SeqID[match(anbei$CaseID_Index2, nodes$CaseID)]

anbei$sequencedCase <- 0
anbei$sequencedCase[anbei$index1_seqID != "" | anbei$index2_seqID != ""] <- 0
anbei$sequencedCase[anbei$index1_seqID != "" & anbei$index2_seqID != ""] <- 2

anbei$CaseID_Index_start <- NULL
anbei$CaseID_Kontaktperson_start <- NULL

##remove duplicates
anbei2 <- anbei[,which(names(anbei) %in% c("indexSearch", "Typ","sequencedCase"))]
anbei2 <- unique(anbei2)


genGen <- anbei2[anbei2$sequencedCase == 2,]


genGen$index1 <- as.character(lapply(genGen$indexSearch, function(x){ strsplit(x, " ")[[1]][[1]]}))
genGen$index2 <- as.character(lapply(genGen$indexSearch, function(x){ strsplit(x, " ")[[1]][[2]]}))


genGen$index1_seqID <- nodes$SeqID[match(genGen$index1, nodes$CaseID)]
genGen$index2_seqID <- nodes$SeqID[match(genGen$index2, nodes$CaseID)]


genGen$Genetic_Distance2 <- lapply(seq_len(nrow(genGen)), function(x){
   
   c1 <- genGen$index1_seqID[x]
   c2 <- genGen$index2_seqID[x]
   
   getGeneticDistance(c1, c2)
   
})

genGen$Genetic_Distance2 <- as.numeric(genGen$Genetic_Distance2)

contacts$Genetic_Distance2 <- genGen$Genetic_Distance2[match(contacts$indexSearch, genGen$indexSearch)]

contacts <- contacts[!is.na(contacts$Genetic_Distance2),]



## Bar Plot der Infektionskontexte innerhalb der KP-P?rchen mit Abstand 0 oder 1

gencon <- contacts

gencon$Infektionskontext_engl <- gencon$Infektionskontext
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Kindergarten"] <- "Kindergarten/daycare"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Betreuung"] <- "Kindergarten/daycare"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Kita"] <- "Kindergarten/daycare"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Asyl/Obdach"] <- "Refugee accommodation"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Freizeit"] <- "Recreational context"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "privates Treffen"] <- "Recreational context"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Verpflegung"] <- "Recreational context"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "häusliches Umfeld"] <- "Private household"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "häusliches Umfeld/Reise"] <- "Private household"
gencon$Infektionskontext_engl[is.na(gencon$Infektionskontext)] <- "not available"


table(gencon$Infektionskontext_engl)

gencon$best_Genetic_Distance <- gencon$Genetic_Distance2



### context pair rejected or Accepted

gencon$genetic_validation <- ""
gencon$genetic_validation[gencon$best_Genetic_Distance < 2] <- "Accepted"
gencon$genetic_validation[gencon$best_Genetic_Distance > 1] <- "Rejected"

gencon$Infektionskontext_engl <- gencon$Infektionskontext
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Kindergarten"] <- "Kindergarten/daycare"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Betreuung"] <- "Kindergarten/daycare"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Kita"] <- "Kindergarten/daycare"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Freizeit"] <- "Recreational context"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "privates Treffen"] <- "Recreational context"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Verpflegung"] <- "Recreational context"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "häusliches Umfeld"] <- "Private household"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "häusliches Umfeld/Reise"] <- "Private household"
gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("Altenheim")] <- "Care home"
gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("ambulante Pflege")] <- "Private household"
gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("Obdachlosenunterkunft","ambulante WG")] <- "Residential Care Facility"
gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("Altenheim"  )] <- "Care home"
gencon$Infektionskontext_engl[gencon$Infektionskontext ==  "Tagesmutter"] <- "Kindergarten/daycare"
gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("Asylheim" ,
                                                              "Asylunterkunft",  "Asyl/Obdach")] <- "Refugee accommodation"
gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("Krankenhaus",
                                                              "Station?r, KH")] <- "Hospital"  



gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("Flugzeug", "MaiTai-Ausbruch")] <- "Recreational context"
gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("Arbeit", "Arbeitsplatz")] <- "Work"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "privater Haushalt"] <- "Private household"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Schule"] <- "School"

table(gencon$Infektionskontext_engl)
#103/sum(table(gencon$Infektionskontext_engl))
table(gencon$Infektionskontext_engl)/sum(table(gencon$Infektionskontext_engl))

res <- data.frame(table(gencon$genetic_validation, gencon$Infektionskontext_engl))

res$Var1 <- as.character(res$Var1)

res$category <- factor(res$Var1, levels= c("Accepted", "Rejected"))
#res$Freq[res$Var1 == "not available"] <- savevalue

res$percentage <- as.character(paste0(round(res$Freq/sum(res$Freq)*100,1), "%"))

res$groupCount <- 0
res$groupCount[res$Var2 == "Kindergarten/daycare"] <- sum(res$Freq[res$Var2 == "Kindergarten/daycare"] )
res$groupCount[res$Var2 == "Private household"] <- sum(res$Freq[res$Var2 == "Private household"] )
res$groupCount[res$Var2 == "Refugee accommodation"] <- sum(res$Freq[res$Var2 == "Refugee accommodation"] )
res$groupCount[res$Var2 == "Recreational context"] <- sum(res$Freq[res$Var2 == "Recreational context"] )

res$percentage <- as.character(paste0(round(res$Freq/res$groupCount *100,1), "%"))

res <- res[res$Var2 != "not available",]

res$Var2  <- as.character(res$Var2 )

res$Var2[res$Var2 == "Kindergarten/daycare"] <- "Kindergarten/\ndaycare"
res$Var2[res$Var2 == "Recreational context"] <- "Recreational\ncontext"
res$Var2[res$Var2 == "Private household"] <- "Private\nhousehold"
res$Var2[res$Var2 == "Refugee accommodation"] <- "Refugee\naccommodation"

res$Var2 <- factor(res$Var2, levels=c("Private\nhousehold","Recreational\ncontext",
                                      "Kindergarten/\ndaycare","Refugee\naccommodation"))

res$percentage[res$percentage == "Inf%"] <- "100%"
res$percentage[res$percentage == "NaN%"] <- "0%"

gInfectedBy <- ggplot(data=res, aes(x=Var2, y=Freq, fill = category)) +
   geom_bar(stat="identity",position = "dodge")+ 
   #theme(axis.text.x = element_text(angle = 52, hjust=1,size=12))+
   theme(axis.text.x = element_text(size=12))+
   geom_text(aes(label = percentage), position= position_dodge(width = .9), vjust = -0.2, hjust= .5, size=3)+
   xlab("")+ylab("Number of Backward contact tracing pairs")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")

gInfectedBy

##ggsave(g, filename=paste0("Fig Contact Tracing (Accepted, rejected for InfectedBy).svg"), width=5, height = 4)
getwd()

gencon$Meldedatum_date2 <- NULL
gencon2 <- na.omit(gencon)
chisq.test(gencon2$Infektionskontext_engl, gencon2$genetic_validation)

#p <- 0.1547  ## => context not significantly different

gencon2 <- data.frame(Infektionskontext_engl= gencon$Infektionskontext_engl, genetic_validation= gencon$genetic_validation)
chisq.test(gencon2$Infektionskontext_engl, gencon2$genetic_validation)
#p <- 0.8412  ## => context not significantly different

gencon2 <- gencon2[gencon2$Infektionskontext_engl %in% c("Private household","Recreational context"),]
chisq.test(gencon2$Infektionskontext_engl, gencon2$genetic_validation)

#p-value = 1
#p-value = 0.3976

fisher.test(gencon2$Infektionskontext_engl, gencon2$genetic_validation)







## Same for contact pairs

library(readxl)
contacts <- read_excel("contact person mit Gen. Info.xlsx", 
                       sheet = "contact person")

contacts$CaseID_Index1 <- as.character(gsub(" ", "", contacts$CaseID_Index1))
contacts$CaseID_Index2 <- as.character(gsub(" ", "", contacts$CaseID_Index2))
contacts$CaseID_Index1 <- as.character(gsub("[\r\n]", "", contacts$CaseID_Index1))
contacts$CaseID_Index2 <- as.character(gsub("[\r\n]", "", contacts$CaseID_Index2))

contacts$CaseID_Index1 <- as.character(lapply(contacts$CaseID_Index1, function(x){
   
   strsplit(x, "\\(")[[1]][[1]]
   
}))

contacts$CaseID_Index2 <- as.character(lapply(contacts$CaseID_Index2, function(x){
   
   strsplit(x, "\\(")[[1]][[1]]
   
}))

contacts <- data.frame(contacts)

contacts$CaseID <- ""
contacts$CaseID_Kontaktperson <- ""

edgesGraphContact <- edgesGraph[edgesGraph$Typ == "SurvnetKontaktperson",]
edgesGraphContact <- unique(edgesGraphContact)

contacts$indexSearch <- as.character(lapply(1:nrow(contacts), function(x){
   
   i1 <- sort(c(contacts$CaseID_Index2[x], contacts$CaseID_Index1[x]))[1]
   i2 <- sort(c(contacts$CaseID_Index2[x], contacts$CaseID_Index1[x]))[2]
   
   paste(i1,i2)
   
}))

contacts$indexSearch <- paste(contacts$indexSearch, "SurvnetKontaktperson")

contacts <- contacts[contacts$indexSearch %in% edgesGraphContact$indexSearch,]
contacts <- contacts[contacts$DatumsDifferenz <= 14 ,]

#contacts <- contacts[abs(contacts$DatumsDifferenz) <= 14,]
anbei <- contacts

anbei$index1_seqID <- nodes$SeqID[match(anbei$CaseID_Index1, nodes$CaseID)]
anbei$index2_seqID <- nodes$SeqID[match(anbei$CaseID_Index2, nodes$CaseID)]

anbei$sequencedCase <- 0
anbei$sequencedCase[anbei$index1_seqID != "" | anbei$index2_seqID != ""] <- 0
anbei$sequencedCase[anbei$index1_seqID != "" & anbei$index2_seqID != ""] <- 2

anbei$CaseID_Index_start <- NULL
anbei$CaseID_Kontaktperson_start <- NULL

##remove duplicates
anbei2 <- anbei[,which(names(anbei) %in% c("indexSearch", "Typ","sequencedCase"))]
anbei2 <- unique(anbei2)


genGen <- anbei2[anbei2$sequencedCase == 2,]


genGen$index1 <- as.character(lapply(genGen$indexSearch, function(x){ strsplit(x, " ")[[1]][[1]]}))
genGen$index2 <- as.character(lapply(genGen$indexSearch, function(x){ strsplit(x, " ")[[1]][[2]]}))


genGen$index1_seqID <- nodes$SeqID[match(genGen$index1, nodes$CaseID)]
genGen$index2_seqID <- nodes$SeqID[match(genGen$index2, nodes$CaseID)]


genGen$Genetic_Distance2 <- lapply(seq_len(nrow(genGen)), function(x){
   
   c1 <- genGen$index1_seqID[x]
   c2 <- genGen$index2_seqID[x]
   
   getGeneticDistance(c1, c2)
   
})

genGen$Genetic_Distance2 <- as.numeric(genGen$Genetic_Distance2)

contacts$Genetic_Distance2 <- genGen$Genetic_Distance2[match(contacts$indexSearch, genGen$indexSearch)]

contacts <- contacts[!is.na(contacts$Genetic_Distance2),]

# gencon2 <- gencon2[gencon2$Infektionskontext_engl %in% c("Private household","Recreational context"),]
# chisq.test(gencon2$Infektionskontext_engl, gencon2$genetic_validation)
# p-value = 0.3976


## Bar Plot der Infektionskontexte innerhalb der KP-P?rchen mit Abstand 0 oder 1

gencon <- contacts

gencon$best_Genetic_Distance <- gencon$Genetic_Distance2
## Bar Plot der Infektionskontexte innerhalb der KP-P?rchen mit Abstand 0 oder 1
gencon$genetic_validation <- ""
gencon$genetic_validation[gencon$best_Genetic_Distance < 2] <- "Accepted"
gencon$genetic_validation[gencon$best_Genetic_Distance > 1] <- "Rejected"

gencon$Infektionskontext_engl <- gencon$Infektionskontext
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Kindergarten"] <- "Kindergarten/daycare"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Betreuung"] <- "Kindergarten/daycare"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Kita"] <- "Kindergarten/daycare"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Freizeit"] <- "Recreational context"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "privates Treffen"] <- "Recreational context"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Verpflegung"] <- "Recreational context"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "häusliches Umfeld"] <- "Private household"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "häusliches Umfeld/Reise"] <- "Private household"
gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("Altenheim")] <- "Care home"
gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("ambulante Pflege")] <- "Private household"
gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("Obdachlosenunterkunft","ambulante WG")] <- "Residential Care Facility"
gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("Altenheim"  )] <- "Care home"
gencon$Infektionskontext_engl[gencon$Infektionskontext ==  "Tagesmutter"] <- "Kindergarten/daycare"
gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("Asylheim" ,
                                                              "Asylunterkunft",  "Asyl/Obdach")] <- "Refugee accommodation"
gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("Krankenhaus",
                                                              "Stationär, KH")] <- "Hospital"  



gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("Flugzeug", "MaiTai-Ausbruch")] <- "Recreational context"
gencon$Infektionskontext_engl[gencon$Infektionskontext %in% c("Arbeit", "Arbeitsplatz")] <- "Work"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "privater Haushalt"] <- "Private household"
gencon$Infektionskontext_engl[gencon$Infektionskontext == "Schule"] <- "School"


table(gencon$Infektionskontext_engl)
643/sum(table(gencon$Infektionskontext_engl))
72/sum(table(gencon$Infektionskontext_engl))

gencon$Infektionskontext_engl[gencon$Infektionskontext_engl == "Care home"] <- "Other"
gencon$Infektionskontext_engl[gencon$Infektionskontext_engl == "Refugee accommodation"] <- "Other"
gencon$Infektionskontext_engl[gencon$Infektionskontext_engl == "Residential Care Facility"] <- "Other"


## add new context
KPcontexts <- read.csv("KP Kontaktumfeld 20230321.csv", sep=";")

uniqueIDs <- unique(KPcontexts$Kontaktumfeld)
fallListe <- list()

for(i in uniqueIDs){
   
   curTab <- KPcontexts[KPcontexts$Kontaktumfeld == i,]
   
   
   #if(nrow(curTabFall) < 1 | nrow(curTabNoFall) < 1 ){
   if(nrow(curTab) < 1 ){
      print("non found for ")
      print(i)
   }else{
      
      y <- rbind(curTab[1,])
      fallListe[[paste0("Kontext:",i)]] <- y
      
   }
   
   
}

library(data.table)
fallListe <- rbindlist(fallListe)


gencon$Infektionskontext_engl2 <- KPcontexts$Kontaktumfeld[match(gencon$Kontaktperson_Ort, KPcontexts$Aktenzeichen )]

test <- data.frame(ID=gencon$Infektionskontext_engl2, manual=gencon$Infektionskontext)
test <- unique(test)
test$searchID <- paste(test$ID, test$manual)
gencon$searchID <- paste(gencon$Infektionskontext_engl2, manual=gencon$Infektionskontext)
test2 <- table(gencon$searchID)
test$freq <- test2[match(test$searchID, names(test2))]

#write.csv2(test, file="test.csv", row.names = F)



res <- data.frame(table(gencon$genetic_validation, gencon$Infektionskontext_engl))

res$Var1 <- as.character(res$Var1)

res$category <- factor(res$Var1, levels= c("Accepted", "Rejected"))

res$percentage <- as.character(paste0(round(res$Freq/sum(res$Freq)*100,1), "%"))

res$groupCount <- 0
res$groupCount[res$Var2 == "Kindergarten/daycare"] <- sum(res$Freq[res$Var2 == "Kindergarten/daycare"] )
res$groupCount[res$Var2 == "Private household"] <- sum(res$Freq[res$Var2 == "Private household"] )
res$groupCount[res$Var2 == "Refugee accommodation"] <- sum(res$Freq[res$Var2 == "Refugee accommodation"] )
res$groupCount[res$Var2 == "Recreational context"] <- sum(res$Freq[res$Var2 == "Recreational context"] )
res$groupCount[res$Var2 == "Care home"] <- sum(res$Freq[res$Var2 == "Care home"] )
res$groupCount[res$Var2 == "Hospital"] <- sum(res$Freq[res$Var2 == "Hospital"] )
res$groupCount[res$Var2 == "Residential Care Facility"] <- sum(res$Freq[res$Var2 == "Residential Care Facility"] )
res$groupCount[res$Var2 == "School"] <- sum(res$Freq[res$Var2 == "School"] )
res$groupCount[res$Var2 == "Work"] <- sum(res$Freq[res$Var2 == "Work"] )

res$groupCount[res$Var2 == "Other"] <- sum(res$Freq[res$Var2 == "Other"] )



res$percentage <- as.character(paste0(round(res$Freq/res$groupCount *100,1), "%"))

res <- res[res$Var2 != "not available",]

res$Var2  <- as.character(res$Var2 )

res$Var2[res$Var2 == "Kindergarten/daycare"] <- "Kindergarten/\nDaycare"
res$Var2[res$Var2 == "Recreational context"] <- "Recreational\nContext"
res$Var2[res$Var2 == "Private household"] <- "Private\nHousehold"
res$Var2[res$Var2 == "Refugee accommodation"] <- "Refugee\nAccommodation"
res$Var2[res$Var2 == "Care home"] <- "Care\nhome"
res$Var2[res$Var2 == "Residential Care Facility"] <- "Residential\nCare Facility"



res$Var2  <- factor(res$Var2, levels= c("Private\nHousehold","Recreational\nContext","Kindergarten/\nDaycare",
                                        "Work","School", "Hospital","Residential\nCare Facility",
                                        "Refugee\nAccommodation",  "Care\nHome","Kindergarten", "Other"))

library(ggplot2)
g<- ggplot(data=res, aes(x=Var2, y=Freq, fill = category)) +
   geom_bar(stat="identity",position = "dodge")+ 
   #theme(axis.text.x = element_text(angle = 52, hjust=1,size=12))+
   theme(axis.text.x = element_text(size=15))+
   geom_text(aes(label = percentage), position= position_dodge(width = .9), vjust = -0.2, hjust= .5, size=5)+
   xlab("")+ylab("Number of contact pairs")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")

g


getwd()

gencon$Meldedatum_date2 <- NULL

gencon2 <- data.frame(Infektionskontext_engl= gencon$Infektionskontext_engl, genetic_validation= gencon$genetic_validation)
chisq.test(gencon2$Infektionskontext_engl, gencon2$genetic_validation)
p <- 0.984  ## => context not significantly different

gencon2 <- gencon2[gencon2$Infektionskontext_engl %in% c("Private household","Recreational context"),]
chisq.test(gencon2$Infektionskontext_engl, gencon2$genetic_validation)
#p-value =  0.4567

test <- fisher.test(gencon2$Infektionskontext_engl, gencon2$genetic_validation)
## p= 0.4195

gencon2 <- gencon2[!gencon2$Infektionskontext_engl %in% c("School", "Work" ),]
chisq.test(gencon2$Infektionskontext_engl, gencon2$genetic_validation)
#with same categories as infected by not significant


g <- g +
   theme(text = element_text(size=15))+
   ylab("Number of forward contact tracing pairs")

g <- g +theme(
   # text = element_text(family = "Arial"),
   axis.text.x = element_text( size = 15),#,angle = 45, hjust=1),
   axis.text.y = element_text(size = 15),
   axis.title = element_text(size = 18),
   # legend.text = element_text(family = "Arial", size = 15),
   plot.background = element_rect(fill = "white", color = NA),
   panel.background = element_rect(fill = "white", color = NA),
   panel.grid.major = element_line(color = "grey80"),
   panel.grid.minor = element_blank(),
   axis.ticks = element_blank(),
   plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
)

g

gInfectedBy <- gInfectedBy +
   theme(text = element_text(size=15))+ylab("Number of backward contact tracing pairs")



#ggsave(g, file="Figure 3 D.svg" , width=11.7, height=5)
#ggsave(gInfectedBy, file="Supplementary Figure 10.svg" , width=6.5, height=4.5)




###  Venn Diagram


## check genetic distance of sameAddress and sameAddressSameName
edge4 <- edge3
edge4$index1Seq <- nodes$SeqID[match(edge4$index1, nodes$CaseID)]
edge4$index2Seq <- nodes$SeqID[match(edge4$index2, nodes$CaseID)]


#edge4 <- edge4[edge4$Typ %in% c("SelbeAdresse", "SelbeAdresseNachname"),]
#edge4 <- edge4[edge4$index1Seq != "" & edge4$index2Seq != "",]

edge4$distance <- as.numeric(lapply(1:nrow(edge4), function(x){
   
   getGeneticDistance(edge4$index1Seq[x], edge4$index2Seq[x])
   
}))

edge4$typ2 <- edge4$Typ 
edge4$indexSearch <- paste(edge4$index1, edge4$index2)
edge4$typ2[!edge4$indexSearch %in% edge4$indexSearch[edge4$typ2 == "SelbeAdresseNachname"] ] <- "SameAddressDifferentName"

table(edge4$Typ)

edgesGraph$indexSearch2 <- paste(edgesGraph$index1, edgesGraph$index2)

edge4Venn <- list("Same address"= edgesGraph$indexSearch2[edgesGraph$Typ== "SelbeAdresse"],
                  "Same address and name"= edgesGraph$indexSearch2[edgesGraph$Typ== "SelbeAdresseNachname"],
                  "Backward contact tracing"= edgesGraph$indexSearch2[edgesGraph$Typ== "SurvnetAngestecktBei"],
                  "Forward contact tracing"= edgesGraph$indexSearch2[edgesGraph$Typ== "SurvnetKontaktperson"])

library(ggVennDiagram)
g <- ggVennDiagram(edge4Venn)+
   scale_fill_gradient(low = "white", high = "white") +  # Grayscale fill
   theme_void() +                                       # Optional minimalist theme
   theme(legend.position = "none")  
g
##ggsave(g, filename="Supplementary Figure 11.svg", width = 8, height = 6)





## check overlapp of same address and outbreak edges
edge4 <- edge3
edge4$index1Seq <- nodes$SeqID[match(edge4$index1, nodes$CaseID)]
edge4$index2Seq <- nodes$SeqID[match(edge4$index2, nodes$CaseID)]


edge4$typ2 <- edge4$Typ 
edge4$indexSearch <- paste(edge4$index1, edge4$index2)
edge4$typ2[!edge4$indexSearch %in% edge4$indexSearch[edge4$typ2 == "SelbeAdresseNachname"] ] <- "SameAddressDifferentName"

table(edge4$Typ)

edgesGraph$indexSearch2 <- paste(edgesGraph$index1, edgesGraph$index2)

edge4Venn <- list("Same address"= edgesGraph$indexSearch2[edgesGraph$Typ== "SelbeAdresse"],
                  "Same address and name"= edgesGraph$indexSearch2[edgesGraph$Typ== "SelbeAdresseNachname"],
                  "Same outbreak"= edgesGraph$indexSearch2[edgesGraph$Typ== "Outbreak"])

library(ggVennDiagram)
library(ggplot2)
g <- ggVennDiagram(edge4Venn)+
   scale_fill_gradient(low = "white", high = "white") +  # Grayscale fill
   theme_void() +                                       # Optional minimalist theme
   theme(legend.position = "none")  
g
#ggsave(g, filename="Supplementary Figure 11.svg", width = 8, height = 6)







## check genetic distance of sameAddress and sameAddressSameName
edge4 <- edge3
edge4$index1Seq <- nodes$SeqID[match(edge4$index1, nodes$CaseID)]
edge4$index2Seq <- nodes$SeqID[match(edge4$index2, nodes$CaseID)]


edge4 <- edge4[edge4$Typ %in% c("SelbeAdresse", "SelbeAdresseNachname"),]
edge4 <- edge4[edge4$index1Seq != "" & edge4$index2Seq != "",]

edge4$distance <- as.numeric(lapply(1:nrow(edge4), function(x){
   
   getGeneticDistance(edge4$index1Seq[x], edge4$index2Seq[x])
   
}))

edge4$distanceGroup <- edge4$distance  
edge4$distanceGroup[edge4$distance > 1] <- "rejected"
edge4$distanceGroup[edge4$distance < 2] <- "accepted"

edge4$typ2 <- edge4$Typ 
edge4$indexSearch <- paste(edge4$index1, edge4$index2)
edge4$typ2[!edge4$indexSearch %in% edge4$indexSearch[edge4$typ2 == "SelbeAdresseNachname"] ] <- "SameAddressDifferentName"

edge4$c <- 1
edge4TBL <- tapply(edge4$c[edge4$typ2 == "SameAddressDifferentName"], edge4$distanceGroup[edge4$typ2 == "SameAddressDifferentName" ], sum)
diffName <- data.frame(edge4TBL)
diffName$dist <- row.names(diffName)
diffName$percent <- round(diffName$edge4TBL / sum(diffName$edge4TBL)*100)
diffName$type <- "SameAddressDifferentName"

edge4TBL <- tapply(edge4$c[edge4$typ2 == "SelbeAdresseNachname"], edge4$distanceGroup[edge4$typ2 == "SelbeAdresseNachname" ], sum)
SameName <- data.frame(edge4TBL)
SameName$dist <- row.names(SameName)
SameName$percent <- round(SameName$edge4TBL / sum(SameName$edge4TBL)*100)
SameName$type <- "SelbeAdresseNachname"


all <- rbind(SameName, diffName)


data <- matrix(
   c(1535 , 291 , 1017 , 365 ), # Flattened data: Rejected and Accepted for A1, then for A2
   nrow = 2,          # Number of rows (conditions)
   byrow = TRUE       # Fill matrix row-wise
)

# Assign row and column names for clarity
rownames(data) <- c("SelbeAdresseNachname", "SameAddressDifferentName")
colnames(data) <- c( "Accepted","Rejected")

# Perform chi-squared test
chi_result <- chisq.test(data)

# Display the result
print(chi_result)

#X-squared = 52.413, df = 1, p-value = 4.497e-13

## Do remaining suppl figures

vaccinated_once <- nodes2[nodes2$Impfung_Anzahl %in% c(1),]
vaccinated_more <- nodes2[nodes2$Impfung_Anzahl %in% c(2,3),]

vaccinated_once$n_contacts_geneticallySame <- lapply(vaccinated_once$CaseID, function(x){
   
   cur_edge <- edges[edges$CaseID_Index == x,]
   cur_edge <- cur_edge[cur_edge$Typ == "SurvnetKontaktperson",]
   
   if(nrow(cur_edge) != 0){
      
      cur_edge$seq1 <- nodes2$SeqID[nodes2$CaseID == cur_edge$CaseID_Index[1]]
      cur_edge$seq2 <- nodes2$SeqID[match(cur_edge$CaseID_Kontaktperson,nodes2$CaseID)]
      
      cur_edge$Genetic_Distance <- lapply(1:nrow(cur_edge), function(xx){
         
         getGeneticDistance(cur_edge$seq1[xx], cur_edge$seq2[xx])
         
      })
      
      return(sum(cur_edge$Genetic_Distance < 2, na.rm=T))
   }else{
      
      
      return(NA)
   }
   
})

vaccinated_once$n_contacts_geneticallySame  <- unlist(vaccinated_once$n_contacts_geneticallySame )
vaccinated_once_vec <- na.omit(vaccinated_once$n_contacts_geneticallySame  )


vaccinated_more$n_contacts_geneticallySame <- lapply(vaccinated_more$CaseID, function(x){
   
   cur_edge <- edges[edges$CaseID_Index == x,]
   cur_edge <- cur_edge[cur_edge$Typ == "SurvnetKontaktperson",]
   
   if(nrow(cur_edge) != 0){
      
      cur_edge$seq1 <- nodes2$SeqID[nodes2$CaseID == cur_edge$CaseID_Index[1]]
      cur_edge$seq2 <- nodes2$SeqID[match(cur_edge$CaseID_Kontaktperson,nodes2$CaseID)]
      
      cur_edge$Genetic_Distance <- lapply(1:nrow(cur_edge), function(xx){
         
         getGeneticDistance(cur_edge$seq1[xx], cur_edge$seq2[xx])
         
      })
      
      return(sum(cur_edge$Genetic_Distance < 2, na.rm=T))
   }else{
      
      
      return(NA)
   }
   
})

vaccinated_more$n_contacts_geneticallySame  <- unlist(vaccinated_more$n_contacts_geneticallySame )
vaccinated_more_vec <- na.omit(vaccinated_more$n_contacts_geneticallySame  )


vaccinated_more_vec <- as.numeric(vaccinated_more_vec)
vaccinated_once_vec <- as.numeric(vaccinated_once_vec)

t.test(vaccinated_once_vec, vaccinated_more_vec)


vaccinated_more$relative_contacts_geneticallySame <- vaccinated_more$n_contacts_geneticallySame/vaccinated_more$Count_Kontaktpersonen_Duesseldorf 
vaccinated_once$relative_contacts_geneticallySame <- vaccinated_once$n_contacts_geneticallySame/vaccinated_once$Count_Kontaktpersonen_Duesseldorf 


vaccinated_more_vec2 <- na.omit(vaccinated_more$relative_contacts_geneticallySame  )
vaccinated_once_vec2 <- na.omit(vaccinated_once$relative_contacts_geneticallySame  )


vaccinated_more_vec2 <- as.numeric(vaccinated_more_vec2)
vaccinated_once_vec2 <- as.numeric(vaccinated_once_vec2)

vaccinated_more_vec2[vaccinated_more_vec2 != 0] <- 1
vaccinated_once_vec2[vaccinated_once_vec2 != 0] <- 1

t.test(vaccinated_once_vec2, vaccinated_more_vec2)

table(vaccinated_more_vec2)
table(vaccinated_once_vec2)

data <- matrix(
   c(162, 78 , 61, 38), # Flattened data: Rejected and Accepted for A1, then for A2
   nrow = 2,          # Number of rows (conditions)
   byrow = TRUE       # Fill matrix row-wise
)

# Assign row and column names for clarity
rownames(data) <- c("More than once vaccinated", "Once vaccinated")
colnames(data) <- c( "No infected contacts", "Infected contacts")

# Perform chi-squared test
chi_result <- chisq.test(data)
# X-squared = 0.83242, df = 1, p-value = 0.3616

data <- table(out$outbreakType)

# Convert table to a data frame
df <- as.data.frame(data)
colnames(df) <- c("Category", "Count")  # Rename columns

df$Category <- as.character(df$Category)

df$Category[df$Category == "Care home"] <- "Care\nhome"
df$Category[df$Category == "Kindergarten/daycare"] <- "Kindergarten/\ndaycare"
df$Category[df$Category == "Recreational context"] <- "Recreational\ncontext"
df$Category[df$Category == "Refugee Accommodation"] <- "Refugee\nAccommodation"
df$Category[df$Category == "Residential Care Facility"] <- "Residential\nCare Facility"


df$Category2 <- factor(df$Category, levels= c("School", "Kindergarten/\ndaycare",  "Hospital",
                                              "Care\nhome" , "Work",  "Nightlife"  , "Residential\nCare Facility",
                                              "Recreational\ncontext", "Refugee\nAccommodation","Other"  ))

# Create the bar plot
g <- ggplot(df, aes(x = Category2, y = Count)) +
   geom_bar(stat = "identity") +  # Use "identity" to plot actual values
   theme_minimal() +  # Apply a minimal theme
   labs(title = "Bar Plot from Table", x = "Category", y = "Count")+
   theme(
      text = element_text(size = 15)) # Increase all text size



## Subsampling sequencing data
nodes_subsampling <- data.frame(CaseID=nodes$CaseID, sequenced = nodes$SeqID != "")
sequencedCaseIDs <- nodes2$CaseID

#20, 15, 10, 5, 2.5, 1 percent

## 1 percent sind 32830/100 * 1

set1 <- unique(sample(sequencedCaseIDs, ceiling(32830/100)))
set2 <- unique(sample(sequencedCaseIDs, ceiling(32830/100)))
set3 <- unique(sample(sequencedCaseIDs, ceiling(32830/100)))
set4 <- unique(sample(sequencedCaseIDs, ceiling(32830/100)))
set5 <- unique(sample(sequencedCaseIDs, ceiling(32830/100)))

nodes_subsampling$subsample_1pc_rep1 <- 0
nodes_subsampling$subsample_1pc_rep2 <- 0
nodes_subsampling$subsample_1pc_rep3 <- 0
nodes_subsampling$subsample_1pc_rep4 <- 0
nodes_subsampling$subsample_1pc_rep5 <- 0

nodes_subsampling$subsample_1pc_rep1[nodes_subsampling$CaseID %in% set1] <- 1 
nodes_subsampling$subsample_1pc_rep2[nodes_subsampling$CaseID %in% set2] <- 1
nodes_subsampling$subsample_1pc_rep3[nodes_subsampling$CaseID %in% set3] <- 1
nodes_subsampling$subsample_1pc_rep4[nodes_subsampling$CaseID %in% set4] <- 1
nodes_subsampling$subsample_1pc_rep5[nodes_subsampling$CaseID %in% set5] <- 1

rm(set1, set2, set3, set4, set5)



## 2.5 percent sind 32830/100 * 2.5

set1 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*2.5)))
set2 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*2.5)))
set3 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*2.5)))
set4 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*2.5)))
set5 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*2.5)))

nodes_subsampling$subsample_2.5pc_rep1 <- 0
nodes_subsampling$subsample_2.5pc_rep2 <- 0
nodes_subsampling$subsample_2.5pc_rep3 <- 0
nodes_subsampling$subsample_2.5pc_rep4 <- 0
nodes_subsampling$subsample_2.5pc_rep5 <- 0

nodes_subsampling$subsample_2.5pc_rep1[nodes_subsampling$CaseID %in% set1] <- 1 
nodes_subsampling$subsample_2.5pc_rep2[nodes_subsampling$CaseID %in% set2] <- 1
nodes_subsampling$subsample_2.5pc_rep3[nodes_subsampling$CaseID %in% set3] <- 1
nodes_subsampling$subsample_2.5pc_rep4[nodes_subsampling$CaseID %in% set4] <- 1
nodes_subsampling$subsample_2.5pc_rep5[nodes_subsampling$CaseID %in% set5] <- 1

rm(set1, set2, set3, set4, set5)





## 5 percent sind 32830/100 * 2.5

set1 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*5)))
set2 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*5)))
set3 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*5)))
set4 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*5)))
set5 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*5)))

nodes_subsampling$subsample_5pc_rep1 <- 0
nodes_subsampling$subsample_5pc_rep2 <- 0
nodes_subsampling$subsample_5pc_rep3 <- 0
nodes_subsampling$subsample_5pc_rep4 <- 0
nodes_subsampling$subsample_5pc_rep5 <- 0

nodes_subsampling$subsample_5pc_rep1[nodes_subsampling$CaseID %in% set1] <- 1 
nodes_subsampling$subsample_5pc_rep2[nodes_subsampling$CaseID %in% set2] <- 1
nodes_subsampling$subsample_5pc_rep3[nodes_subsampling$CaseID %in% set3] <- 1
nodes_subsampling$subsample_5pc_rep4[nodes_subsampling$CaseID %in% set4] <- 1
nodes_subsampling$subsample_5pc_rep5[nodes_subsampling$CaseID %in% set5] <- 1

rm(set1, set2, set3, set4, set5)




## 10 percent sind 32830/100 * 10

set1 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*10)))
set2 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*10)))
set3 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*10)))
set4 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*10)))
set5 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*10)))

nodes_subsampling$subsample_10pc_rep1 <- 0
nodes_subsampling$subsample_10pc_rep2 <- 0
nodes_subsampling$subsample_10pc_rep3 <- 0
nodes_subsampling$subsample_10pc_rep4 <- 0
nodes_subsampling$subsample_10pc_rep5 <- 0

nodes_subsampling$subsample_10pc_rep1[nodes_subsampling$CaseID %in% set1] <- 1 
nodes_subsampling$subsample_10pc_rep2[nodes_subsampling$CaseID %in% set2] <- 1
nodes_subsampling$subsample_10pc_rep3[nodes_subsampling$CaseID %in% set3] <- 1
nodes_subsampling$subsample_10pc_rep4[nodes_subsampling$CaseID %in% set4] <- 1
nodes_subsampling$subsample_10pc_rep5[nodes_subsampling$CaseID %in% set5] <- 1

rm(set1, set2, set3, set4, set5)





## 15 percent sind 32830/100 * 15

set1 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*15)))
set2 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*15)))
set3 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*15)))
set4 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*15)))
set5 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*15)))

nodes_subsampling$subsample_15pc_rep1 <- 0
nodes_subsampling$subsample_15pc_rep2 <- 0
nodes_subsampling$subsample_15pc_rep3 <- 0
nodes_subsampling$subsample_15pc_rep4 <- 0
nodes_subsampling$subsample_15pc_rep5 <- 0

nodes_subsampling$subsample_15pc_rep1[nodes_subsampling$CaseID %in% set1] <- 1 
nodes_subsampling$subsample_15pc_rep2[nodes_subsampling$CaseID %in% set2] <- 1
nodes_subsampling$subsample_15pc_rep3[nodes_subsampling$CaseID %in% set3] <- 1
nodes_subsampling$subsample_15pc_rep4[nodes_subsampling$CaseID %in% set4] <- 1
nodes_subsampling$subsample_15pc_rep5[nodes_subsampling$CaseID %in% set5] <- 1

rm(set1, set2, set3, set4, set5)




## 20 percent sind 32830/100 * 20

set1 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*20)))
set2 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*20)))
set3 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*20)))
set4 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*20)))
set5 <- unique(sample(sequencedCaseIDs, ceiling(32830/100*20)))

nodes_subsampling$subsample_20pc_rep1 <- 0
nodes_subsampling$subsample_20pc_rep2 <- 0
nodes_subsampling$subsample_20pc_rep3 <- 0
nodes_subsampling$subsample_20pc_rep4 <- 0
nodes_subsampling$subsample_20pc_rep5 <- 0

nodes_subsampling$subsample_20pc_rep1[nodes_subsampling$CaseID %in% set1] <- 1 
nodes_subsampling$subsample_20pc_rep2[nodes_subsampling$CaseID %in% set2] <- 1
nodes_subsampling$subsample_20pc_rep3[nodes_subsampling$CaseID %in% set3] <- 1
nodes_subsampling$subsample_20pc_rep4[nodes_subsampling$CaseID %in% set4] <- 1
nodes_subsampling$subsample_20pc_rep5[nodes_subsampling$CaseID %in% set5] <- 1

rm(set1, set2, set3, set4, set5)

#write.csv2(nodes_subsampling, file="Subsampling Dataset.csv",row.names=F)


nodes_subsampling <- read.csv("/mock/Subsampling Dataset.csv", sep=";")
## Make plots about contact tracing pairs with sequencing data
edge2$seqID1 <- nodes_subsampling$sequenced[match(edge2$CaseID_Index,nodes_subsampling$CaseID )]
edge2$seqID2 <- nodes_subsampling$sequenced[match(edge2$CaseID_Kontaktperson,nodes_subsampling$CaseID )]

edge2$pair_sequenced <- 0
edge2$pair_sequenced[edge2$seqID1 == 1 & edge2$seqID2 == 1] <- 1

table(edge2$Typ, edge2$pair_sequenced)


## Make it automatic!

subSam <- data.frame(caseSequencingRate = c(rep(c(1,2.5,5,10,15,20),each=5),24.45),
                     replicate= c(rep(c(1,2,3,4,5),6),1), columnID = c(3:32,2), value= 0)
subSam2 <- subSam
subSam3 <- subSam

### iterate over all sets
for(i in 1:nrow(subSam)){
   
   columnIDor <- subSam$columnID[i]
   
   edge2$seqID1 <- 0
   edge2$seqID2 <- 0
   edge2$seqID1 <- nodes_subsampling[match(edge2$CaseID_Index,nodes_subsampling$CaseID), columnIDor]
   edge2$seqID2 <- nodes_subsampling[match(edge2$CaseID_Kontaktperson,nodes_subsampling$CaseID),columnIDor]
   
   edge2$pair_sequenced <- 0
   edge2$pair_sequenced[edge2$seqID1 == 1 & edge2$seqID2 == 1] <- 1
   
   resultsTable <- table(edge2$Typ, edge2$pair_sequenced)
   resultsTable <- data.frame(resultsTable)
   resultsTable$total[resultsTable$Var1 == "SelbeAdresse"] <- sum(resultsTable$Freq[resultsTable$Var1 == "SelbeAdresse"])
   resultsTable$total[resultsTable$Var1 == "SurvnetAngestecktBei"] <- sum(resultsTable$Freq[resultsTable$Var1 == "SurvnetAngestecktBei"])
   resultsTable$total[resultsTable$Var1 == "SurvnetKontaktperson"] <- sum(resultsTable$Freq[resultsTable$Var1 == "SurvnetKontaktperson"])
   
   resultsTable$percentage <- round(resultsTable$Freq/resultsTable$total*100,2)
   resultsTable <- resultsTable[resultsTable$Var2 == 1,]
   resultsTable <- na.omit(resultsTable)
   
   subSam$value[i] <- resultsTable$percentage[resultsTable$Var1 == "SelbeAdresse"]
   subSam2$value[i] <- resultsTable$percentage[resultsTable$Var1 == "SurvnetAngestecktBei"]
   subSam3$value[i] <- resultsTable$percentage[resultsTable$Var1 == "SurvnetKontaktperson"]
}


subSam$type <- "Same address"
subSam2$type <- "Backward contact tracing"
subSam3$type <- "Forward contact tracing"










library(ggplot2)
library(dplyr)

# Combine all your datasets into one (assuming your dataframes are subSam, subSam2, subSam3)
# Add the 'type' column to each of them beforehand
combined_data <- bind_rows(subSam, subSam2, subSam3)

# Calculate the mean value for each caseSequencingRate and type combination for the lines
aggregated_data <- combined_data %>%
   group_by(caseSequencingRate, type) %>%
   summarize(mean_value = mean(value), .groups = 'drop')

# Create the plot
g <-ggplot(combined_data, aes(x = caseSequencingRate, y = value, color = type)) +
   # Plot the individual points
   geom_point(size = 3) +
   # Plot the lines using the aggregated data
   geom_line(data = aggregated_data, aes(x = caseSequencingRate, y = mean_value, color = type), size = 1) +
   labs(
      
      x = "Case Sequencing Rate [%]",
      y = "Case pairs with genetic dinstance [%]",
      color = "Type"
   ) +
   #theme_minimal() +         # Clean theme
   theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
   ) +
   scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB"))  # Custom colors for each type





g <- g+
   scale_x_continuous(
      breaks = c(0, 5, 10, 15, 20, 25),
      labels = function(x) paste0(x, "%")
   )

g <- g  +
   scale_x_reverse(
      breaks = c(0, 5, 10, 15, 20, 25),
      labels = function(x) paste0(x, "%"))
g

#ggsave(g, filename="Amount of case pairs with genentic distance available upon subsampling.svg", width=8, height = 5)





## Do additional upsampling


subSam <- data.frame(caseSequencingRate = rep(seq(30,100,10),each=5),
                     replicate=rep(c(1,2,3,4,5),8), columnID = 0, value= 0)
subSam2 <- subSam
subSam3 <- subSam

allIDs <- nodes_subsampling$CaseID
allIDs_notSeq <- allIDs[!allIDs %in% nodes_subsampling$CaseID[nodes_subsampling$sequenced]]
allIDs_Seq <- allIDs[allIDs %in% nodes_subsampling$CaseID[nodes_subsampling$sequenced]]
seqCaseIDsLength <- length(nodes_subsampling$CaseID[nodes_subsampling$sequenced])

documentCaseIDsUpsampling <- list()

### iterate over all sets
for(i in 1:nrow(subSam)){
   
   curPercent <- subSam$caseSequencingRate[i]
   
   numberOfsequences <- ceiling(32830/100*curPercent)
   numberOfsequences_stillNeeded <- numberOfsequences-seqCaseIDsLength
   
   subset_allIDs_notSeq <- unique(sample(allIDs_notSeq, numberOfsequences_stillNeeded))
   
   combinedCaseIDs_actualAndUpsampled <- c(subset_allIDs_notSeq, allIDs_Seq)
   documentCaseIDsUpsampling[[paste(curPercent, ", rep:", subSam$replicate[i])]] <- combinedCaseIDs_actualAndUpsampled
   
   edge2$seqID1 <- 0
   edge2$seqID2 <- 0
   
   edge2$seqID1[edge2$CaseID_Index %in%  combinedCaseIDs_actualAndUpsampled] <- 1
   edge2$seqID2[edge2$CaseID_Kontaktperson %in%  combinedCaseIDs_actualAndUpsampled]  <- 1
   
   edge2$pair_sequenced <- 0
   edge2$pair_sequenced[edge2$seqID1 == 1 & edge2$seqID2 == 1] <- 1
   
   resultsTable <- table(edge2$Typ, edge2$pair_sequenced)
   resultsTable <- data.frame(resultsTable)
   resultsTable$total[resultsTable$Var1 == "SelbeAdresse"] <- sum(resultsTable$Freq[resultsTable$Var1 == "SelbeAdresse"])
   resultsTable$total[resultsTable$Var1 == "SurvnetAngestecktBei"] <- sum(resultsTable$Freq[resultsTable$Var1 == "SurvnetAngestecktBei"])
   resultsTable$total[resultsTable$Var1 == "SurvnetKontaktperson"] <- sum(resultsTable$Freq[resultsTable$Var1 == "SurvnetKontaktperson"])
   
   resultsTable$percentage <- round(resultsTable$Freq/resultsTable$total*100,2)
   resultsTable <- resultsTable[resultsTable$Var2 == 1,]
   resultsTable <- na.omit(resultsTable)
   
   subSam$value[i] <- resultsTable$percentage[resultsTable$Var1 == "SelbeAdresse"]
   subSam2$value[i] <- resultsTable$percentage[resultsTable$Var1 == "SurvnetAngestecktBei"]
   subSam3$value[i] <- resultsTable$percentage[resultsTable$Var1 == "SurvnetKontaktperson"]
}


subSam$type <- "Same address"
subSam2$type <- "Backward contact tracing"
subSam3$type <- "Forward contact tracing"



combined_data <- bind_rows(combined_data, subSam, subSam2, subSam3)

save(documentCaseIDsUpsampling,combined_data, file="CaseIDs upon upsampling2")
#load("CaseIDs upon upsampling2")

# Calculate the mean value for each caseSequencingRate and type combination for the lines
aggregated_data <- combined_data %>%
   group_by(caseSequencingRate, type) %>%
   summarize(mean_value = mean(value), .groups = 'drop')



# Split aggregated_data into two parts: before and after 30%
aggregated_data_solid <- aggregated_data[aggregated_data$caseSequencingRate < 30, ]
aggregated_data_dotted <- aggregated_data[aggregated_data$caseSequencingRate >= 30, ]

g <- ggplot(combined_data, aes(x = caseSequencingRate, y = value, color = type)) +
   # Plot the individual points
   geom_point(size = 2) +
   # Solid line before 30%
   geom_line(data = aggregated_data_solid,
             aes(x = caseSequencingRate, y = mean_value, color = type),
             size = 1, linetype = "solid") +
   # Dotted line after 30%
   geom_line(data = aggregated_data_dotted,
             aes(x = caseSequencingRate, y = mean_value, color = type),
             size = 1, linetype = "dotted") +
   labs(
      x = "Cases sequenced [%]",
      y = "Case pairs with genetic distance [%]",
      color = "Type"
   ) +
   theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
   ) +
   scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB"))  # Custom colors for each type


g <- g  +
   scale_x_continuous(
      breaks = c(0, 25, 50, 75, 100),
      labels = function(x) paste0(x, "%"))

g <- g  +
   scale_y_continuous(
      breaks = c(0, 25, 50, 75, 100),
      labels = function(x) paste0(x, "%"))
g <- g  + geom_vline(xintercept = 25, linetype = "dashed", color = "black")
g
##ggsave(g, filename="Amount of case pairs with genentic distance available upon sub- and upsampling.svg", width=6.3, height = 3.6)
#ggsave(g, filename="Figure 7 A.svg", width=6.3, height = 3.6)


## Get subsampling data for outbreaks
## load nodes_subsampling object
nodes_subsampling <- read.csv("/mock/Subsampling Dataset.csv", sep=";")

subSam <- data.frame(caseSequencingRate = c(rep(c(1,2.5,5,10,15,20),each=5),24.45),
                     replicate= c(rep(c(1,2,3,4,5),6),1), columnID = c(3:32,2), value= 0)
subSam2 <- subSam

### iterate over all sets
for(i in 1:nrow(subSam)){
   
   columnIDor <- subSam$columnID[i]
   
   ## Calcualte new outbreak number with distance available given sequencing rate
   outbreakClonality <- lapply(out$outbreakID, function(x){
      
      cases <- c(outbreaks$Aktenzeichen[outbreaks$AusbruchInfo_NameGA == x])
      seq_cases_count <- sum(nodes_subsampling[match(cases,nodes_subsampling$CaseID), columnIDor])
      seq_cases_count > 1
   })
   
   outbreaks_with_distance <- sum(unlist(outbreakClonality))
   
   
   ## Calculate number of genetically linked community cases given sequencing rate
   
   outbreakGeneticallyLinked <- lapply(out$outbreakID, function(x){
      
      cases <- out$genLinkedCommunityCasesCaseIDs[out$outbreakID == x]
      cases <- getCaseID(cases)
      sum(nodes_subsampling[match(cases,nodes_subsampling$CaseID), columnIDor])
      
   })
   
   
   subSam$value[i] <- outbreaks_with_distance
   subSam2$value[i] <- sum(unlist(outbreakGeneticallyLinked), na.rm=T)
   
}


# subSam$type <- "Outbreaks with genetic distance"
# subSam2$type <- "Number of genetically linked cases"
subSam$type <- "Backward contact tracing"
subSam2$type <- "Backward contact tracing"



library(ggplot2)
library(dplyr)

# Combine all your datasets into one (assuming your dataframes are subSam, subSam2, subSam3)
# Add the 'type' column to each of them beforehand
combined_data <- bind_rows(subSam, subSam2)
combined_data <- subSam2

# Calculate the mean value for each caseSequencingRate and type combination for the lines
aggregated_data <- combined_data %>%
   group_by(caseSequencingRate, type) %>%
   summarize(mean_value = mean(value), .groups = 'drop')


# Create the plot
g <-ggplot(combined_data, aes(x = caseSequencingRate, y = value, color = type)) +
   # Plot the individual points
   geom_point(size = 3) +
   # Plot the lines using the aggregated data
   geom_line(data = aggregated_data, aes(x = caseSequencingRate, y = mean_value, color = type), size = 1) +
   labs(
      
      x = "Cases sequenced [%]",
      y = "Number of community cases\ngenetically linked to outbreak cases",
      color = "Type"
   ) +
   theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
   ) +
   scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB"))  # Custom colors for each type



g <- g  +
   scale_x_reverse( breaks = c(0, 5, 10, 15, 20, 25),
                    labels = function(x) paste0(x, "%"))

g
##ggsave(g, filename="Outbreak with number genetic realted community cases upon subsampling.svg", width=6.4, height = 3.6)
#ggsave(g, filename="Figure 7 C.svg", width=6.4, height = 3.6)




combined_data <- subSam

##add upsampling
subSam2 <- data.frame(caseSequencingRate = rep(seq(30,100,10),each=5),
                      replicate=rep(c(1,2,3,4,5),8), columnID = 0, value= 0)

allIDs <- nodes_subsampling$CaseID
allIDs_notSeq <- allIDs[!allIDs %in% nodes_subsampling$CaseID[nodes_subsampling$sequenced]]
allIDs_Seq <- allIDs[allIDs %in% nodes_subsampling$CaseID[nodes_subsampling$sequenced]]
seqCaseIDsLength <- length(nodes_subsampling$CaseID[nodes_subsampling$sequenced])

### iterate over all sets
for(i in 1:nrow(subSam2)){
   
   curPercent <- subSam2$caseSequencingRate[i]
   
   numberOfsequences <- ceiling(32830/100*curPercent)
   numberOfsequences_stillNeeded <- numberOfsequences-seqCaseIDsLength
   
   subset_allIDs_notSeq <- unique(sample(allIDs_notSeq, numberOfsequences_stillNeeded))
   
   combinedCaseIDs_actualAndUpsampled <- c(subset_allIDs_notSeq, allIDs_Seq)
   
   ## Calcualte new outbreak number with distance available given sequencing rate
   outbreakClonality <- lapply(out$outbreakID, function(x){
      
      cases <- c(outbreaks$Aktenzeichen[outbreaks$AusbruchInfo_NameGA == x])
      seq_cases_count <- sum(cases %in% combinedCaseIDs_actualAndUpsampled)
      seq_cases_count > 1
   })
   
   outbreaks_with_distance <- sum(unlist(outbreakClonality))
   
   subSam2$value[i] <- outbreaks_with_distance
   
}


subSam2$type <- "Backward contact tracing"



combined_data <- rbind(subSam,subSam2)

combined_data$value <- combined_data$value/max(combined_data$value)*100

# Calculate the mean value for each caseSequencingRate and type combination for the lines
aggregated_data <- combined_data %>%
   group_by(caseSequencingRate, type) %>%
   summarize(mean_value = mean(value), .groups = 'drop')


# Split aggregated_data into two parts: before and after 30%
aggregated_data_solid <- aggregated_data[aggregated_data$caseSequencingRate < 30, ]
aggregated_data_dotted <- aggregated_data[aggregated_data$caseSequencingRate >= 30, ]


# Create the plot
g <-ggplot(combined_data, aes(x = caseSequencingRate, y = value, color = type)) +
   # Plot the individual points
   geom_point(size = 3) +
   # Plot the lines using the aggregated data
   # Solid line before 30%
   geom_line(data = aggregated_data_solid,
             aes(x = caseSequencingRate, y = mean_value, color = type),
             size = 1, linetype = "solid") +
   # Dotted line after 30%
   geom_line(data = aggregated_data_dotted,
             aes(x = caseSequencingRate, y = mean_value, color = type),
             size = 1, linetype = "dotted") +
   
   labs(
      
      x = "Cases sequenced [%]",
      y = "Outbreaks with > 1 sequenced\ncases [%]",
      color = "Type"
   ) +
   theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
   ) +
   scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB"))  # Custom colors for each type



g <- g  +
   scale_x_continuous(
      breaks = c(0, 25, 50, 75, 100),
      labels = function(x) paste0(x, "%"))
g <- g  + geom_vline(xintercept = 25, linetype = "dashed", color = "black")

g <- g  +
   scale_y_continuous(
      breaks = c(0, 25, 50, 75, 100),
      labels = function(x) paste0(x, "%"))

g
##ggsave(g, filename="Number of outbreaks with genetic distance available upon subsampling.svg", width=6.5, height =3.6)
#ggsave(g, filename="Figure 7 B.svg", width=6.5, height = 3.6)





## infection source per subsampling
inSource <- read.delim("/mock/infsources_per_sequenced_cases_connected_components_0.tsv")

inSource$subsampling[inSource$subsampling == "all"] <- "24.45%"
inSource$subsampling <- gsub("%", "", inSource$subsampling)
inSource$subsampling  <- as.numeric(inSource$subsampling)
inSource$type <-  "Backward contact tracing"

inSource$inf_sources_per_seq_cases <- inSource$inf_sources_per_seq_cases*100

aggregated_data <- inSource %>%
   group_by(subsampling, type) %>%
   summarize(mean_value = mean(inf_sources_per_seq_cases), .groups = 'drop')

g <-ggplot(inSource, aes(x = subsampling, y = inf_sources_per_seq_cases, color = type)) +
   # Plot the individual points
   geom_point(size = 3) +
   # Plot the lines using the aggregated data
   geom_line(data = aggregated_data, aes(x = subsampling , y = mean_value, color = type), size = 1) +
   labs(
      
      x = "Cases sequenced [%]",
      y = "Sequenced cases with putative\ninfection source [%]",
      color = "Type"
   ) +
   theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
   ) +
   scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB"))  # Custom colors for each type



g <- g   +
   scale_x_reverse( breaks = c(0, 5, 10, 15, 20, 25),
                    labels = function(x) paste0(x, "%"))

g <- g+ scale_y_continuous(breaks = c(0, 5, 10, 15, 20),
                           labels = function(x) paste0(x, "%"))
g
##ggsave(g, filename="Number of Cases with infectionsource per sequenced cases upon subsampling.svg", width=6.43, height = 3.6)
#ggsave(g, filename="Figure 7 D.svg", width=6.43, height = 3.6)



## Putative Transmission rate per context ID

##First filter by index case of our time of interest
KPcontexts <-  read.delim("/mock/KP and context.csv", comment.char="#")

nrow(KPcontexts) #54148
KPcontexts <- KPcontexts[KPcontexts$IndexFall_Token %in% nodes$CaseID,]
nrow(KPcontexts) #47601

KPpos <- KPcontexts

KPpos$Aktenzeichen <- lapply(KPpos$Aktenzeichen, function(x) {
   if (is.character(x)) {
      gsub("[\r\n]", "", x)
   } else {
      x
   }
})

KPpos$IndexFall_Token <- lapply(KPpos$IndexFall_Token, function(x) {
   if (is.character(x)) {
      gsub("[\r\n]", "", x)
   } else {
      x
   }
})

edge$CaseID_Index <- lapply(edge$CaseID_Index, function(x) {
   if (is.character(x)) {
      gsub("[\r\n]", "", x)
   } else {
      x
   }
})

edge$Kontaktperson_Ort <- lapply(edge$Kontaktperson_Ort, function(x) {
   if (is.character(x)) {
      gsub("[\r\n]", "", x)
   } else {
      x
   }
})



contacts <- edge2[edge2$Typ == "SurvnetKontaktperson",]
contacts$KontextSearchID <- paste(contacts$CaseID_Index,",", contacts$Kontaktperson_Ort)

KPpos$searchID <- paste(KPpos$IndexFall_Token,",", KPpos$Aktenzeichen)

##save contactsContext
edge2$KontextSearchID <- paste(edge2$CaseID_Index,",", edge2$Kontaktperson_Ort)
conti <- read_excel("Supplementary Table Translation of german context terms.xlsx")

KPpos$context <- conti$`Translated context label`[match(KPpos$Kontaktumfeld, conti$`Survnet context label (German)`)]
edge2$context <- KPpos$context[match(edge2$KontextSearchID, KPpos$searchID)]

infectedByWithGeneticInfo <- read_excel("/mock/AngestecktBei pairs (1).xlsx", 
                                        sheet = "pairs with genetic info")

infectedByNoGeneticInfo <- read_excel("/mock/AngestecktBei pairs (1).xlsx", 
                                      sheet = "pairs without genetic info")
infectedBy <- rbind(infectedByWithGeneticInfo, infectedByNoGeneticInfo)

infectedBy$Infektionskontext_engl <- infectedBy$Infektionskontext
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Kindergarten"] <- "Kindergarten/daycare"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Betreuung"] <- "Kindergarten/daycare"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Kita"] <- "Kindergarten/daycare"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Freizeit"] <- "Recreational context"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "privates Treffen"] <- "Recreational context"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Verpflegung"] <- "Recreational context"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "häusliches Umfeld"] <- "Private household"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "häusliches Umfeld/Reise"] <- "Private household"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext %in% c("Altenheim")] <- "Care home"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext %in% c("ambulante Pflege")] <- "Residential Care Facility"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext %in% c("Obdachlosenunterkunft","ambulante WG")] <- "Residential Care Facility"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext %in% c("Altenheim"  )] <- "Care home"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext ==  "Tagesmutter"] <- "Kindergarten/daycare"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext %in% c("Asylheim" ,
                                                                      "Asylunterkunft",  "Asyl/Obdach")] <- "Refugee accommodation"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext %in% c("Krankenhaus",
                                                                      "Station?r, KH")] <- "Hospital"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "privater Haushalt"] <- "Private household"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "privater Haushalt/Altenheim"] <- "Care home"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "nicht nachvollziehbar"] <- "Others"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Schule"] <- "School"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Arbeitsplatz"] <- "Work"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Asyl"] <- "Refugee accommodation"

table(infectedBy$Infektionskontext_engl)

infectedBy$index1 <- as.character(lapply(1:nrow(infectedBy), function(x){
   
   i1 <- infectedBy$CaseID_Index2[x]
   i2 <- infectedBy$CaseID_Index1[x]
   
   ids <- sort(c(i1, i2))
   ids[1]
   
}))

infectedBy$index2 <- as.character(lapply(1:nrow(infectedBy), function(x){
   
   i1 <- infectedBy$CaseID_Index2[x]
   i2 <- infectedBy$CaseID_Index1[x]
   
   ids <- sort(c(i1, i2))
   ids[2]
   
}))

infectedBy$indexSearch <- paste(infectedBy$index1, infectedBy$index2)
infectedBy$indexSearch <- paste(infectedBy$indexSearch, infectedBy$Typ)

edge2$context_manual <- infectedBy$Infektionskontext_engl[match(edge2$indexSearch, infectedBy$indexSearch)]

write.csv2(edge2, file="Edges mit Kontext.csv", row.names=F)


sum(KPpos$searchID %in% contacts$KontextSearchID)
sum(contacts$KontextSearchID %in% KPpos$searchID)

sum(!KPpos$searchID %in% contacts$KontextSearchID)
sum(!contacts$KontextSearchID %in% KPpos$searchID)


KPpos$IndexFall_Meldedatum2 <- substr(KPpos$IndexFall_Meldedatum, 1, 10)
KPpos_classification <- KPpos
KPpos <- KPpos[!KPpos$IndexFall_Meldedatum2 %in% paste0(c(18:31), ".12.2021"),]

##filter only contexts with address in duesseldorf
KPpos$c <- 1
cont <- data.frame(tapply(KPpos$c,KPpos$PersonOrt, sum))
cont$ID <- row.names(cont)
names(cont) <- c("freq", "ID")


#install.packages("stringdist")
library(stringdist)

cont$ID <- iconv(cont$ID, from = "latin1", to = "UTF-8")
cont$name2 <- as.character(lapply(cont$ID, function(x){substr(x, 1,1)}))
cont$c <- 1:nrow(cont)
cont$search2 <- 999

# Get city name and compare it with stingdist
for(i in 1:length(cont$ID)){
   
   x <- cont$ID[i]
   y <- gsub("[0-9]", "", x)
   y <- strsplit(y, " ")[[1]]
   y <- tolower(y)
   
   # Calculate string distance from a known correct name
   distances <- stringdist(y, "duesseldorf")
   
   cont$search2[i]  <- min(distances)
   
}

cont$rigthCity[cont$search2 <= 2 | cont$ID %in% c("40233", "40472", "40597",
                                                  "Duesseldorfschueler","Duess")] <- "Duesseldorf"

sum(cont$freq[cont$rigthCity == "Duesseldorf"], na.rm=T)

contVec <- cont$ID[cont$rigthCity == "Duesseldorf" ]
contVec <- na.omit(contVec)
contVec <- as.character(contVec)

## still 39477

KPpos$contactAddressDuesseldorf <- 0
KPpos$contactAddressDuesseldorf[KPpos$PersonOrt %in% contVec] <- 1

table(KPpos$contactAddressDuesseldorf)

#  0     1 
# 8124 39477 
KPpos <- KPpos[KPpos$contactAddressDuesseldorf == 1,]


##remove contexts where index ID not found
contacts <- contacts[contacts$CaseID_Index %in% KPpos$IndexFall_Token ,]


contacts$Kontaktperson_Ort <-as.character(contacts$Kontaktperson_Ort)
KPpos$Aktenzeichen <-as.character(KPpos$Aktenzeichen)
KPpos_classification$Aktenzeichen <-as.character(KPpos_classification$Aktenzeichen)

KPpos$Aktenzeichen <- gsub("[\t\r\n]", "", KPpos$Aktenzeichen)
KPpos_classification$Aktenzeichen <- gsub("[\t\r\n]", "", KPpos_classification$Aktenzeichen)


contacts$contactOrt <- KPpos$PersonOrt[match(contacts$Kontaktperson_Ort, KPpos$Aktenzeichen)]

##cp became positive
KPpos$cpPositive <- 0
KPpos$cpPositive[KPpos$Aktenzeichen %in% contacts$Kontaktperson_Ort] <- 1


KPpos$c <- 1
conti <- data.frame(tapply(KPpos$c,KPpos$Kontaktumfeld, sum))
conti$ID <- row.names(conti)
names(conti) <- c("freq", "ID")

write.csv2(conti, file="KPcontext_summi.csv", row.names=F)

library(readxl)
conti <- read_excel("Supplementary Table Translation of german context terms.xlsx")

KPpos$context <- conti$`Translated context label`[match(KPpos$Kontaktumfeld, conti$`Survnet context label (German)`)]
KPpos_classification$context <- conti$`Translated context label`[match(KPpos_classification$Kontaktumfeld, conti$`Survnet context label (German)`)]



## Get number of contacts, that became positive
cont2 <- data.frame(tapply(KPpos$c, KPpos$context, sum))
cont2$x <- row.names(cont2)

names(cont2) <- c("freq", "context")
row.names(cont2) <- NULL
cont2 <- cont2[,c(2,1)]

cont2$pos_cont <- 0

cont2$n_indexCases <- 0
cont2$n_indexCases_with_positiveContacts <- 0

cont2$edges_with_genetic_Distance <- 0
cont2$edges_with_genetic_Distance_accepted <- 0

cont2$pos_cont_unique <- 0
cont2$edges_with_genetic_Distance_unique <- 0

for(id in cont2$context){
   
   kps <- KPpos[KPpos$context == id,]
   
   kpse <- contacts
   kpse <- kpse[kpse$Kontaktperson_Ort %in% kps$Aktenzeichen,]
   cont2$pos_cont[cont2$context == id] <- nrow(kpse)
   if(nrow(kpse) != 0) kpse$uniqueID <- paste0(kpse$index1, ",", kpse$index2)
   cont2$pos_cont_unique[cont2$context == id] <- length(unique(kpse$uniqueID))
   cont2$n_indexCases[cont2$context == id] <- length(unique(kps$IndexFall_Token))
   cont2$n_indexCases_with_positiveContacts[cont2$context == id] <- length(unique(kpse$CaseID_Index))
   
   ##Check sequencing data
   kpse$Index_SeqID <- nodes$SeqID[match(kpse$CaseID_Index, nodes$CaseID)]
   kpse$Contact_SeqID <- nodes$SeqID[match(kpse$CaseID_Kontaktperson, nodes$CaseID)]
   
   kpse2 <- kpse[kpse$Index_SeqID != "" & kpse$Contact_SeqID != "",]
   
   if(nrow(kpse2)>0){
      kpse2$Genetic_Distance <- as.numeric(lapply(1:nrow(kpse2), function(x){
         
         
         getGeneticDistance(kpse2$Index_SeqID[x], kpse2$Contact_SeqID[x])
      }))}
   
   cont2$edges_with_genetic_Distance[cont2$context == id] <- nrow(kpse2)
   cont2$edges_with_genetic_Distance_unique[cont2$context == id] <- length(unique(kpse2$uniqueID))
   cont2$edges_with_genetic_Distance_accepted[cont2$context == id] <- nrow(kpse2[kpse2$Genetic_Distance <=1,])
   
   
}


cont2$transmission_rate <- round(cont2$pos_cont/cont2$freq*100, 1)


write.csv2(cont2, file="Supplementary Table 3.csv", row.names=F)





##First filter by index case of our time of interest
KPcontexts <-  read.delim("/mock/KP and context.csv", comment.char="#")

nrow(KPcontexts) #54148
KPcontexts <- KPcontexts[KPcontexts$IndexFall_Token %in% nodes$CaseID,]
nrow(KPcontexts) #47601



KPpos <- KPcontexts

KPpos$Aktenzeichen <- lapply(KPpos$Aktenzeichen, function(x) {
   if (is.character(x)) {
      gsub("[\r\n]", "", x)
   } else {
      x
   }
})

KPpos$IndexFall_Token <- lapply(KPpos$IndexFall_Token, function(x) {
   if (is.character(x)) {
      gsub("[\r\n]", "", x)
   } else {
      x
   }
})


contacts <- edge2[edge2$Typ == "SurvnetKontaktperson",]
contacts$KontextSearchID <- paste(contacts$CaseID_Index,",", contacts$Kontaktperson_Ort)

KPpos$searchID <- paste(KPpos$IndexFall_Token,",", KPpos$Aktenzeichen)

##save contactsContext
edge2$KontextSearchID <- paste(edge2$CaseID_Index,",", edge2$Kontaktperson_Ort)
conti <- read_excel("Supplementary Table Translation of german context terms.xlsx")

KPpos$context <- conti$`Translated context label`[match(KPpos$Kontaktumfeld, conti$`Survnet context label (German)`)]
edge2$context <- KPpos$context[match(edge2$KontextSearchID, KPpos$searchID)]



KPpos$IndexFall_Meldedatum2 <- substr(KPpos$IndexFall_Meldedatum, 1, 10)

#KPpos <- KPpos[!KPpos$IndexFall_Meldedatum2 %in% paste0(c(18:31), ".12.2021"),]

##filter only contexts with address in duesseldorf
KPpos$c <- 1
cont <- data.frame(tapply(KPpos$c,KPpos$PersonOrt, sum))
cont$ID <- row.names(cont)
names(cont) <- c("freq", "ID")


#install.packages("stringdist")
library(stringdist)

cont$ID <- iconv(cont$ID, from = "latin1", to = "UTF-8")
cont$name2 <- as.character(lapply(cont$ID, function(x){substr(x, 1,1)}))
cont$c <- 1:nrow(cont)
cont$search2 <- 999

# Your column of city names
for(i in 1:length(cont$ID)){
   
   x <- cont$ID[i]
   y <- gsub("[0-9]", "", x)
   y <- strsplit(y, " ")[[1]]
   y <- tolower(y)
   
   # Calculate string distance from a known correct name
   distances <- stringdist(y, "duesseldorf")
   
   cont$search2[i]  <- min(distances)
   
}

cont$rigthCity[cont$search2 <= 2 | cont$ID %in% c("40233", "40472", "40597",
                                                  "Duesseldorfschueler","Duess")] <- "Duesseldorf"

sum(cont$freq[cont$rigthCity == "Duesseldorf"], na.rm=T)

contVec <- cont$ID[cont$rigthCity == "Duesseldorf" ]
contVec <- na.omit(contVec)
contVec <- as.character(contVec)

## still 39477

KPpos$contactAddressDuesseldorf <- 0
KPpos$contactAddressDuesseldorf[KPpos$PersonOrt %in% contVec] <- 1

table(KPpos$contactAddressDuesseldorf)

#  0     1 
# 8124 39477 
KPpos <- KPpos[KPpos$contactAddressDuesseldorf == 1,]

##remove contexts where index ID not found
contacts <- contacts[contacts$CaseID_Index %in% KPpos$IndexFall_Token ,]


any(KPpos$Aktenzeichen == "cpCVD2021-10004")
contacts$Kontaktperson_Ort <-as.character(contacts$Kontaktperson_Ort)
KPpos$Aktenzeichen <-as.character(KPpos$Aktenzeichen)

contacts$contactOrt <- KPpos$PersonOrt[match(contacts$Kontaktperson_Ort, KPpos$Aktenzeichen)]

##cp became positive
KPpos$cpPositive <- 0
KPpos$cpPositive[KPpos$Aktenzeichen %in% contacts$Kontaktperson_Ort] <- 1


KPpos$c <- 1
conti <- data.frame(tapply(KPpos$c,KPpos$Kontaktumfeld, sum))
conti$ID <- row.names(conti)
names(conti) <- c("freq", "ID")

library(readxl)
conti <- read_excel("Supplementary Table Translation of german context terms.xlsx")

KPpos$context <- conti$`Translated context label`[match(KPpos$Kontaktumfeld, conti$`Survnet context label (German)`)]

## Get number of contacts, that became positive
cont2 <- data.frame(tapply(KPpos$c, KPpos$context, sum))
cont2$x <- row.names(cont2)

names(cont2) <- c("freq", "context")
row.names(cont2) <- NULL
cont2 <- cont2[,c(2,1)]

cont2$pos_cont <- 0

cont2$n_indexCases <- 0
cont2$n_indexCases_with_positiveContacts <- 0

cont2$edges_with_genetic_Distance <- 0
cont2$edges_with_genetic_Distance_accepted <- 0

cont2$pos_cont_unique <- 0
cont2$edges_with_genetic_Distance_unique <- 0

for(id in cont2$context){
   
   kps <- KPpos[KPpos$context == id,]
   
   kpse <- contacts
   #kpse <- kpse[kpse$indexSearch %in% edgesGraph$indexSearch[edgesGraph$Typ == "SurvnetKontaktperson"],]
   kpse <- kpse[kpse$Kontaktperson_Ort %in% kps$Aktenzeichen,]
   cont2$pos_cont[cont2$context == id] <- nrow(kpse)
   if(nrow(kpse) != 0) kpse$uniqueID <- paste0(kpse$index1, ",", kpse$index2)
   cont2$pos_cont_unique[cont2$context == id] <- length(unique(kpse$uniqueID))
   cont2$n_indexCases[cont2$context == id] <- length(unique(kps$IndexFall_Token))
   cont2$n_indexCases_with_positiveContacts[cont2$context == id] <- length(unique(kpse$CaseID_Index))
   
   ##Check sequencing data
   kpse$Index_SeqID <- nodes$SeqID[match(kpse$CaseID_Index, nodes$CaseID)]
   kpse$Contact_SeqID <- nodes$SeqID[match(kpse$CaseID_Kontaktperson, nodes$CaseID)]
   
   kpse2 <- kpse[kpse$Index_SeqID != "" & kpse$Contact_SeqID != "",]
   
   if(nrow(kpse2)>0){
      kpse2$Genetic_Distance <- as.numeric(lapply(1:nrow(kpse2), function(x){
         
         
         getGeneticDistance(kpse2$Index_SeqID[x], kpse2$Contact_SeqID[x])
      }))}
   
   cont2$edges_with_genetic_Distance[cont2$context == id] <- nrow(kpse2)
   cont2$edges_with_genetic_Distance_unique[cont2$context == id] <- length(unique(kpse2$uniqueID))
   cont2$edges_with_genetic_Distance_accepted[cont2$context == id] <- nrow(kpse2[kpse2$Genetic_Distance <=1,])
   
   
}


cont2$transmission_rate <- round(cont2$pos_cont/cont2$freq*100, 1)

contactsWithContext <- contacts

write.csv2(cont2, file="Supplementary Table 3_dec31.csv", row.names=F)


cont2$part <-  round(cont2$freq/sum(cont$freq)*100,1)

test <- edge2[edge2$Typ == "SurvnetKontaktperson",]


test <- edgesGraph[edgesGraph$Typ == "SelbeAdresse",]

allCases <- unique(c(test$index1, test$index2))

length(allCases) / nrow(nodes)


cont17 <- read.csv2("Supplementary Table transmission_rate_contexts2.csv")


## Figure transmission rate
rate_s <- data.frame(Context = cont2$context, freq=cont2$freq, pos_cont=cont2$pos_cont) 
rate_s$trans_rate <- cont17$transmission_rate[match(rate_s$Context, cont17$context )]
rate <- rate_s[rate_s$freq >= 200,]

rate$forwardRecords <- rate$freq
rate$forwardRecords_pos <- rate$pos_cont

rate$Context[rate$Context =="Private Houshold"] <- "Private\nHoushold"
rate$Context[rate$Context =="Recreational context"] <- "Recreational\nContext"
rate$Context[rate$Context =="Kindergarten/Daycare"] <- "Kindergarten/ \nDaycare"
rate$Context[rate$Context =="Residential Care Facility"] <- "Residential\nCare Facility"

rate1 <- data.frame(Context=rate$Context, count=rate$forwardRecords, type="All documented records", transmissionRate="")
rate2 <- data.frame(Context=rate$Context, count=rate$forwardRecords_pos, type="Putative transmission events", transmissionRate= paste0(rate$trans_rate,"%"))
rate3 <- rbind(rate1, rate2)

rate <- rate[order(rate$freq, decreasing = T),]
rate3$Context <- factor(rate3$Context, levels= c(rate$Context[rate$Context != "Others"],"Others"))

rate3$count[rate3$type == "All documented records" & rate3$Context == "Others"] <- sum(rate_s$freq[rate_s$freq < 200],rate_s$freq[rate_s$Context %in% c("Others","NA")])
rate3$count[rate3$type == "Putative transmission events" & rate3$Context == "Others"] <- sum(rate_s$pos_cont[rate_s$freq < 200],rate_s$pos_cont[rate_s$Context %in% c("Others","NA")])
rate3$transmissionRate[rate3$type == "Putative transmission events" & rate3$Context == "Others"] <- paste0(round(rate3$count[rate3$type == "Putative transmission events" & rate3$Context == "Others"]/rate3$count[rate3$type == "All documented records" & rate3$Context == "Others"]*100,1 ), "%")
rate3 <- rate3[rate3$Context != "NA",]


library(ggplot2)
library(ggbreak)
library(scales)

# Assume transmissionRate is in percent, e.g., 29.5%
rate3$transRateNum <- as.numeric(gsub("%", "", rate3$transmissionRate))  # remove % and convert

# Add dummy CI (replace with your real lower/upper bounds)
rate3$transRateLow <- as.numeric(lapply(rate3$Context, function(x){ 
   y <- binom.test(rate3$count[rate3$type== "Putative transmission events" & rate3$Context == x ],
                   rate3$count[rate3$type== "All documented records" & rate3$Context == x ])
   y[4][[1]][[1]]
}))
rate3$transRateHigh <-  as.numeric(lapply(rate3$Context, function(x){ 
   y <- binom.test(rate3$count[rate3$type== "Putative transmission events" & rate3$Context == x ],
                   rate3$count[rate3$type== "All documented records" & rate3$Context == x ])
   y[4][[1]][[2]]
}))

rate3$transRateLow  <- rate3$transRateLow*100
rate3$transRateHigh  <- rate3$transRateHigh*100

# Define transformation factor
max_count <- max(rate3$count)
max_percent <- max(rate3$transRateNum, na.rm=T)
scale_factor <- max_count / max_percent  # match percent to count scale

# Create plot
g <- ggplot(rate3, aes(x=Context, y=count, fill=type)) +
   geom_bar(stat="identity", position="identity") +
   scale_fill_brewer(palette="Set2") +
   geom_point(data=subset(rate3, type == "Putative transmission events"),
              aes(y = transRateNum * scale_factor),
              color="black", size=2) +
   geom_errorbar(data=subset(rate3, type == "Putative transmission events"),
                 aes(ymin = transRateLow * scale_factor,
                     ymax = transRateHigh * scale_factor),
                 width = 0.2, color="black") +
   scale_y_continuous(
      name = "Number of forward contact tracing records",
      sec.axis = sec_axis(~ . / scale_factor, name = "Putative Transmission rate [%]", breaks = c(5, 10, 15,20, 30), 
                          labels= c("5%", "10%", "15%", "20%", "30%")),
      breaks = c(1000,2000,4000,6000,8000,10000,12000,14000,16000, 23200,23300)
   ) +
   xlab("Context of forward contact tracing record") +
   #theme_minimal(base_size = 15) +
   theme(
      legend.title = element_blank(),
      #axis.text.x = element_text(angle = 45, hjust = 1),
      axis.ticks.y.right = element_line(),
      axis.text.y.right = element_text(),
      legend.position = "top"
   )

g <- g+ scale_y_break(c(16000, 22000))

g <- g + theme(text = element_text(size=15))

g<-g+theme(
   # text = element_text(family = "Arial"),
   axis.text.x = element_text( size = 15),#,angle = 45, hjust=1),
   axis.text.y = element_text(size = 15),
   axis.title = element_text(size = 18),
   # legend.text = element_text(family = "Arial", size = 15),
   plot.background = element_rect(fill = "white", color = NA),
   panel.background = element_rect(fill = "white", color = NA),
   panel.grid.major = element_line(color = "grey80"),
   panel.grid.minor = element_blank(),
   axis.ticks = element_blank(),
   plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
)
g

##ggsave(g, filename="Figure Putative transmission rate.svg", width=12, height = 5)
#ggsave(g, filename="Figure 3 A", width=12, height = 5)

## 
cases <- edgesGraph$index1[edgesGraph$Typ %in% c("SurvnetAngestecktBei", "SurvnetKontaktperson")]
cases2 <- edgesGraph$index2[edgesGraph$Typ %in% c("SurvnetAngestecktBei", "SurvnetKontaktperson")]
cases3 <- unique(c(cases, cases2))

length(cases3)/nrow(nodes)


infectedByWithGeneticInfo <- read_excel("/mock/AngestecktBei pairs (1).xlsx", 
                                        sheet = "pairs with genetic info")

infectedByNoGeneticInfo <- read_excel("/mock/AngestecktBei pairs (1).xlsx", 
                                      sheet = "pairs without genetic info")
infectedBy <- rbind(infectedByWithGeneticInfo, infectedByNoGeneticInfo)

contacts <- infectedBy
contacts$indexSearch <- as.character(lapply(1:nrow(contacts), function(x){
   
   i1 <- sort(c(contacts$CaseID_Index2[x], contacts$CaseID_Index1[x]))[1]
   i2 <- sort(c(contacts$CaseID_Index2[x], contacts$CaseID_Index1[x]))[2]
   
   paste(i1,i2, "SurvnetAngestecktBei")
   
}))

contacts <- contacts[contacts$indexSearch %in% edgesGraph$indexSearch,]
contacts <- contacts[contacts$DatumsDifferenz <= 14 ,]

#contacts <- contacts[abs(contacts$DatumsDifferenz) <= 14,]
anbei <- contacts

anbei$index1_seqID <- nodes$SeqID[match(anbei$CaseID_Index1, nodes$CaseID)]
anbei$index2_seqID <- nodes$SeqID[match(anbei$CaseID_Index2, nodes$CaseID)]

anbei$sequencedCase <- 0
anbei$sequencedCase[anbei$index1_seqID != "" | anbei$index2_seqID != ""] <- 0
anbei$sequencedCase[anbei$index1_seqID != "" & anbei$index2_seqID != ""] <- 2

anbei$CaseID_Index_start <- NULL
anbei$CaseID_Kontaktperson_start <- NULL

##remove duplicates
anbei2 <- anbei[,which(names(anbei) %in% c("indexSearch", "Typ","sequencedCase"))]
anbei2 <- unique(anbei2)


genGen <- anbei2[anbei2$sequencedCase == 2,]


genGen$index1 <- as.character(lapply(genGen$indexSearch, function(x){ strsplit(x, " ")[[1]][[1]]}))
genGen$index2 <- as.character(lapply(genGen$indexSearch, function(x){ strsplit(x, " ")[[1]][[2]]}))


genGen$index1_seqID <- nodes$SeqID[match(genGen$index1, nodes$CaseID)]
genGen$index2_seqID <- nodes$SeqID[match(genGen$index2, nodes$CaseID)]


genGen$Genetic_Distance2 <- lapply(seq_len(nrow(genGen)), function(x){
   
   c1 <- genGen$index1_seqID[x]
   c2 <- genGen$index2_seqID[x]
   
   getGeneticDistance(c1, c2)
   
})

genGen$Genetic_Distance2 <- as.numeric(genGen$Genetic_Distance2)

contacts$Genetic_Distance2 <- genGen$Genetic_Distance2[match(contacts$indexSearch, genGen$indexSearch)]

contacts <- contacts[!is.na(contacts$Genetic_Distance2),]

infectedBy <- contacts

infectedBy$Infektionskontext_engl <- infectedBy$Infektionskontext
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Kindergarten"] <- "Kindergarten/daycare"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Betreuung"] <- "Kindergarten/daycare"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Kita"] <- "Kindergarten/daycare"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Freizeit"] <- "Recreational context"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "privates Treffen"] <- "Recreational context"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Verpflegung"] <- "Recreational context"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "häusliches Umfeld"] <- "Private household"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "häusliches Umfeld/Reise"] <- "Private household"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext %in% c("Altenheim")] <- "Care home"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext %in% c("ambulante Pflege")] <- "Residential Care Facility"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext %in% c("Obdachlosenunterkunft","ambulante WG")] <- "Residential Care Facility"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext %in% c("Altenheim"  )] <- "Care home"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext ==  "Tagesmutter"] <- "Kindergarten/daycare"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext %in% c("Asylheim" ,
                                                                      "Asylunterkunft",  "Asyl/Obdach")] <- "Refugee accommodation"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext %in% c("Krankenhaus",
                                                                      "Station?r, KH")] <- "Hospital"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "privater Haushalt"] <- "Private household"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "privater Haushalt/Altenheim"] <- "Care home"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "nicht nachvollziehbar"] <- "Others"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Schule"] <- "School"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Arbeitsplatz"] <- "Work"
infectedBy$Infektionskontext_engl[infectedBy$Infektionskontext == "Asyl"] <- "Refugee accommodation"

table(infectedBy$Infektionskontext_engl)

infectedBy$index1 <- as.character(lapply(1:nrow(infectedBy), function(x){
   
   i1 <- infectedBy$CaseID_Index2[x]
   i2 <- infectedBy$CaseID_Index1[x]
   
   ids <- sort(c(i1, i2))
   ids[1]
   
}))

infectedBy$index2 <- as.character(lapply(1:nrow(infectedBy), function(x){
   
   i1 <- infectedBy$CaseID_Index2[x]
   i2 <- infectedBy$CaseID_Index1[x]
   
   ids <- sort(c(i1, i2))
   ids[2]
   
}))

gencon <- infectedBy
gencon$best_Genetic_Distance <- gencon$Genetic_Distance2



### context pair rejected or Accepted

gencon$genetic_validation <- ""
gencon$genetic_validation[gencon$best_Genetic_Distance < 2] <- "Accepted"
gencon$genetic_validation[gencon$best_Genetic_Distance > 1] <- "Rejected"


res <- data.frame(table(gencon$genetic_validation, gencon$Infektionskontext_engl))

res$Var1 <- as.character(res$Var1)

res$category <- factor(res$Var1, levels= c("Accepted", "Rejected"))
#res$Freq[res$Var1 == "not available"] <- savevalue

res$percentage <- as.character(paste0(round(res$Freq/sum(res$Freq)*100,1), "%"))

res$groupCount <- 0
res$groupCount[res$Var2 == "Others"] <- sum(res$Freq[res$Var2 == "Others"] )
res$groupCount[res$Var2 == "Kindergarten/daycare"] <- sum(res$Freq[res$Var2 == "Kindergarten/daycare"] )
res$groupCount[res$Var2 == "Private household"] <- sum(res$Freq[res$Var2 == "Private household"] )
res$groupCount[res$Var2 == "Recreational context"] <- sum(res$Freq[res$Var2 == "Recreational context"] )
res$groupCount[res$Var2 == "Refugee accommodation"] <- sum(res$Freq[res$Var2 == "Refugee accommodation"] )
res$groupCount[res$Var2 == "School"] <- sum(res$Freq[res$Var2 == "School"] )
res$groupCount[res$Var2 == "Work"] <- sum(res$Freq[res$Var2 == "Work"] )

res$percentage <- as.character(paste0(round(res$Freq/res$groupCount *100,1), "%"))

res$Var2  <- as.character(res$Var2 )

res$Var2[res$Var2 == "Private household"] <- "Private\nhousehold"
res$Var2[res$Var2 == "Recreational context"] <- "Recreational\ncontext"
res$Var2[res$Var2 == "Refugee accommodation"] <- "Refugee\naccommodation"
res$Var2[res$Var2 == "Kindergarten/daycare"] <- "Kindergarten/\ndaycare"

res$Var2 <- factor(res$Var2, levels=c("Private\nhousehold","Recreational\ncontext",
                                      "Kindergarten/\ndaycare", "School", "Others","Refugee\naccommodation","Work"))

gInfectedBy <- ggplot(data=res, aes(x=Var2, y=Freq, fill = category)) +
   geom_bar(stat="identity",position = "dodge")+ 
   #theme(axis.text.x = element_text(angle = 52, hjust=1,size=12))+
   theme(axis.text.x = element_text(size=12))+
   geom_text(aes(label = percentage), position= position_dodge(width = .9), vjust = -0.2, hjust= .5, size=3)+
   xlab("")+ylab("Number of backward contact tracing pairs")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")

gInfectedBy

##ggsave(gInfectedBy, file= "Supplementary Figure BackwardContactTracing.svg", width = 6.5, height = 4)
#ggsave(gInfectedBy, file= "Supplementary Figure 10.svg", width = 6.5, height = 4)




# Supplementary Figure ContactTracingGeneticDistancesByContext
#contactsWithContext ## contacts with context
#gencon  infected by with genetic distances and context

table(gencon$Infektionskontext_engl)

table(contactsWithContext$context)


contactsWithContext$seqID1 <- nodes$SeqID[match(contactsWithContext$CaseID_Index, nodes$CaseID)]
contactsWithContext$seqID2 <- nodes$SeqID[match(contactsWithContext$CaseID_Kontaktperson, nodes$CaseID)]


genGen <- contactsWithContext[contactsWithContext$seqID1 != "" & contactsWithContext$seqID2 != "",]

genGen$Genetic_Distance <- lapply(seq_len(nrow(genGen)), function(x){
   
   c1 <- genGen$seqID1[x]
   c2 <- genGen$seqID2[x]
   
   getGeneticDistance(c1, c2)
   
})

genGen$Genetic_Distance <- as.numeric(genGen$Genetic_Distance)
genGen <- genGen[!is.na(genGen$Genetic_Distance),]

##Starting with infected by
gencon$Genetic_Distance2_cat <- gencon$Genetic_Distance2
gencon$Genetic_Distance2_cat[gencon$Genetic_Distance2_cat >= 10] <- ">9"

test <- table(gencon$Genetic_Distance2_cat, gencon$Infektionskontext_engl)
test <- data.frame(test)

test$Var2 <-  as.character(test$Var2) 

test$Var2 <- factor(test$Var2, levels=c("Private household","Recreational context","Kindergarten/daycare", "Refugee accommodation"))
test$Var1 <- factor(test$Var1, levels=c(0:9, ">9"))

library(dplyr)
test_complete <- data.frame(Var1 = factor(c(0:9, ">9"), levels = c(0:9, ">9"))) %>%
   left_join(test, by = "Var1") %>%
   mutate(Freq = ifelse(is.na(Freq), 0, Freq))




library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)

# Ensure Var1 and Var2 are factors
test_complete$Var1 <- factor(test_complete$Var1)
test_complete$Var2 <- factor(test_complete$Var2)

# Create individual plots for each Var2 level
plots <- lapply(levels(test_complete$Var2), function(var_level) {
   data_subset <- filter(test_complete, Var2 == var_level)
   
   plotBreaks <- 0:max(data_subset$Freq )
   
   
   cg <- ggplot(data_subset, aes(Var1, Freq)) +
      geom_bar(stat = "identity", position = "identity") +
      xlab("Genetic distance") +
      ylab("Number of backward contact tracing pairs") +
      ggtitle(paste( var_level)) +
      theme(legend.title = element_blank(),
            text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5)) +
      scale_fill_brewer(palette = "Set2") +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) 
   
   if(max(data_subset$Freq)<10) cg<- cg +
      scale_y_continuous(breaks = 0:max(data_subset$Freq))
   
   cg
   
})

# Arrange in 2 rows x 2 columns
gg3 <- grid.arrange(grobs = plots, nrow = 1, ncol = 4)

##ggsave(gg3, filename="Backward contact tracing genetic distances per context.svg", width=14, height=4)
#ggsave(gg3, filename="Supplementary Figure 7 B.svg", width=14, height=4)





## same for forward contact tracing links
##Starting with infected by
genGen$Genetic_Distance2_cat <- genGen$Genetic_Distance
genGen$Genetic_Distance2_cat[genGen$Genetic_Distance2_cat >= 10] <- ">9"

genGen$context[genGen$context == "NA"] <- "Others"

test <- table(genGen$Genetic_Distance2_cat, genGen$context)
test <- data.frame(test)

test$Var2 <-  as.character(test$Var2) 

# test$Var2[test$Var2 == "SelbeAdresse"] <- "Same address"
# test$Var2[test$Var2 == "SurvnetAngestecktBei"] <- "Backward\ncontact tracing"
# test$Var2[test$Var2 == "SurvnetKontaktperson"] <- "Forward\ncontact tracing"

contextAbundance <- data.frame(table(genGen$context))
contextAbundance <- contextAbundance[order(contextAbundance$Freq, decreasing = T),]


test$Var2 <- factor(test$Var2, levels=contextAbundance$Var1)
test$Var1 <- factor(test$Var1, levels=c(0:9, ">9"))

library(dplyr)
test_complete <- data.frame(Var1 = factor(c(0:9, ">9"), levels = c(0:9, ">9"))) %>%
   left_join(test, by = "Var1") %>%
   mutate(Freq = ifelse(is.na(Freq), 0, Freq))




library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)

# Ensure Var1 and Var2 are factors
test_complete$Var1 <- factor(test_complete$Var1)
test_complete$Var2 <- factor(test_complete$Var2)

# Create individual plots for each Var2 level
plots <- lapply(levels(test_complete$Var2), function(var_level) {
   data_subset <- filter(test_complete, Var2 == var_level)
   
   plotBreaks <- 0:max(data_subset$Freq )
   
   
   cg <- ggplot(data_subset, aes(Var1, Freq)) +
      geom_bar(stat = "identity", position = "identity") +
      xlab("Genetic distance") +
      ylab("Number of forward contact tracing pairs") +
      ggtitle(paste( var_level)) +
      theme(legend.title = element_blank(),
            text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5)) +
      scale_fill_brewer(palette = "Set2") +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) 
   
   if(max(data_subset$Freq)<10) cg<- cg +
      scale_y_continuous(breaks = 0:max(data_subset$Freq))
   
   cg
   
})

# Arrange in 2 rows x 2 columns
gg3 <- grid.arrange(grobs = plots, nrow = 3, ncol = 4)

##ggsave(gg3, filename="Forward contact tracing genetic distances per context.svg", width=14, height=12)
#ggsave(gg3, filename="Supplementary Figure 7 A.svg", width=14, height=4)



## Sequencing rate per week
nodes2$month <- strftime(nodes2$Meldedatum2, format = "%V")
nodes2$count <- 1

geneticsPerMonth <- data.frame(tapply(nodes2$count, nodes2$month, sum))
geneticsPerMonth$date <- row.names(geneticsPerMonth)
geneticsPerMonth$month <- geneticsPerMonth$date

geneticsPerMonth <- geneticsPerMonth[order(geneticsPerMonth$month, decreasing = F),]
monthFactor <- factor(geneticsPerMonth$month, levels=geneticsPerMonth$month)

geneticsPerMonth$monthfactor <- monthFactor

names(geneticsPerMonth) <- c("sequenced_cases","date", "month", "monthfactor")

sequenced <- geneticsPerMonth

nodes$month <- strftime(nodes$Meldedatum2, format = "%V")
nodes$c <- 1
geneticsPerMonth <- data.frame(tapply(nodes$c, nodes$month, sum))
geneticsPerMonth$date <- row.names(geneticsPerMonth)
geneticsPerMonth$month <- geneticsPerMonth$date

geneticsPerMonth <- geneticsPerMonth[order( geneticsPerMonth$month, decreasing = F),]
monthFactor <- factor(geneticsPerMonth$month, levels=geneticsPerMonth$month)

geneticsPerMonth$monthfactor <- monthFactor

names(geneticsPerMonth) <- c("sequenced_cases", "date", "month", "monthfactor")

cases <- geneticsPerMonth


resC <- data.frame(month=cases$monthfactor, count=cases$sequenced_cases, 
                   type="all reported cases")
resS <- data.frame(month=sequenced$monthfactor, count=sequenced$sequenced_cases, 
                   type="sequenced cases")

res <- rbind(resC, resS)

res$test <- c(resC$count,resC$count)
res$percent <- round(res$count/res$test*100)
res$percent[res$percent == 100.0] <- ""
res$percent[res$type != "all reported cases"]  <- paste0(res$percent[res$type != "all reported cases"] , "%")

res$type[res$type=="all reported cases"] <- "All reported cases"
res$type[res$type=="sequenced cases"] <- "Sequenced cases"

library(ggplot2)



g<- ggplot(data=res, aes(x=month, y=count, fill=type)) +
   geom_bar(stat="identity",position = "identity")+ 
   #theme(axis.text.x = element_text(angle = 45, hjust=1))+
   geom_text(aes(label = percent), vjust = -0.2, size=3.5)+
   xlab("Calendar week 2021")+ylab("Number of cases")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+ theme(text = element_text(size=17))+
   theme(
      # text = element_text(family = "Arial"),
      # axis.title = element_text(family = "Arial", size = 15),
      # axis.text.x = element_text(family = "Arial", size = 15,angle = 45, hjust=1),
      # axis.text.y = element_text(family = "Arial", size = 15),
      # legend.text = element_text(family = "Arial", size = 15),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank()
   )
g

##ggsave(g, filename=paste0("Figure Sequenced Cases.svg"), width=16, height = 3)
#ggsave(g, filename=paste0("Figure 2 A.svg"), width=16, height = 3)


contactTracingInfo <- table(edgesGraph$Typ)
contactTracingInfo <- contactTracingInfo[names(contactTracingInfo) != "SelbeAdresse"]

sumRecords <- sum(contactTracingInfo)

contactTracingInfoDF <- data.frame(edge= names(contactTracingInfo), count=as.numeric(contactTracingInfo))
contactTracingInfoDF$percentage <- round(contactTracingInfoDF$count / sumRecords * 100,1)
contactTracingInfoDF$percentage <- paste0(contactTracingInfoDF$percentage, "%")
contactTracingInfoDF <- contactTracingInfoDF[order(contactTracingInfoDF$count, decreasing = F),]

contactTracingInfoDF$edge_engl <- contactTracingInfoDF$edge
contactTracingInfoDF$edge_engl[contactTracingInfoDF$edge_engl == "True_SelbeAdresse"] <- "Same address\n-different name"
contactTracingInfoDF$edge_engl[contactTracingInfoDF$edge_engl == "SelbeAdresseNachname"] <- "Same address\n-same name"
contactTracingInfoDF$edge_engl[contactTracingInfoDF$edge_engl == "SurvnetKontaktperson"] <- "Forward\ncontact tracing"
contactTracingInfoDF$edge_engl[contactTracingInfoDF$edge_engl == "SurvnetAngestecktBei"] <- "Backward\ncontact tracing"

contactTracingInfoDF$edge_engl <- factor(contactTracingInfoDF$edge_engl, levels=contactTracingInfoDF$edge_engl)

g<- ggplot(data=contactTracingInfoDF, aes(x=count, y=edge_engl)) +
   geom_bar(stat="identity",position = "identity")+ 
   #theme(axis.text.x = element_text(angle = 45, hjust=1))+
   geom_text(aes(label = percentage), vjust = +.5, hjust=-.05, size=6)+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+ theme(text = element_text(size=15))+
   theme(
      # text = element_text(family = "Arial"),
      axis.text.x = element_text( size = 15),#,angle = 45, hjust=1),
      axis.text.y = element_text(size = 15),
      axis.title = element_text(size = 20),
      # legend.text = element_text(family = "Arial", size = 15),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(t = 10, r = 30, b = 10, l = 40),
   )+
   coord_cartesian(xlim = c(0, 15500))+
   ylab("")+xlab("Count")
g <- g+
   scale_x_continuous(breaks = seq(0, 14000, by = 2000))

g

##ggsave(g, filename=paste0("Edge count.svg"), width=10, height = 3.5)
#ggsave(gg3, filename="Supplementary Figure 7 B.svg", width=10, height = 3.5)




nodes2$month <- strftime(nodes2$Meldedatum2, format = "%V")
nodes2$count <- 1

nodes2$strain <- SeqID_Ns$Pangolin[match(nodes2$SeqID, SeqID_Ns$ID)]

strains <- data.frame(table(nodes2$strain))
strains$total <- sum(strains$Freq)
strains$percentage <- strains$Freq/strains$total

nodes2$Lineage2 <- "Others"

nodes2$strain <- gsub(" ", "", nodes2$strain)
nodes2$strain2 <- ""

## summarize strains
for(i in 1:nrow(nodes2)){
   
   cur <- nodes2$strain[i]
   
   if(cur == "B.1.1.7") cur <- "B.1.1.7hereA"
   if(cur == "B.1.351") cur <- "B.1.351hereB"
   if(cur == "P.1") cur <- "P.1hereG"
   if(cur == "B.1.617.2") cur <- "B.1.617.2hereD2"
   if(substr(cur, 1,3) == "AY.") cur <- "AY.hereD"
   
   if(cur == "B.1.1.529") cur <- "B.1.1.529hereO"
   if(substr(cur, 1,4) == "BA.1") cur <- "BA.1hereO"
   if(substr(cur, 1,4) == "BA.2") cur <- "BA.2hereO"
   if(substr(cur, 1,4) == "BA.3") cur <- "BA.3hereO"
   if(substr(cur, 1,4) == "BA.4") cur <- "BA.4hereO"
   if(substr(cur, 1,4) == "BA.5") cur <- "BA.5hereO"
   if(substr(cur, 1,4) == "BA.7") cur <- "BA.7hereO"
   if(substr(cur, 1,7) == "BA.2.75") cur <- "BA.2.75hereO"
   if(substr(cur, 1,6) == "BQ.1.1") cur <- "BQ.1.1hereO"
   
   if(substr(cur, 1,7) == "B.1.177") cur <- "B.1.177"
   
   nodes2$strain2[i] <- cur
}

x <- data.frame(table(nodes2$strain2))
x <- x[x$Freq > 10,]

nodes2$Lineage2 <- "Others"
nodes2$Lineage2[nodes2$strain2 %in% x$Var1] <- nodes2$strain2[nodes2$strain2 %in% x$Var1] 

geneticsPerMonth <- data.frame(tapply(nodes2$count, list(nodes2$month, nodes2$Lineage2), sum))

test <- data.frame(week="", Lineage=names(geneticsPerMonth), count=0, percentage=0, total =0)

resList <- list()

for(i in 1:nrow(geneticsPerMonth)){
   
   select <- geneticsPerMonth[i,]
   select[is.na(select)] <- 0
   
   test$count <- as.numeric(select)
   test$week  <- row.names(geneticsPerMonth)[i]
   test$total <- sum(test$count)
   test$percentage <- round((test$count/test$total)*100,1)
   
   resList[[i]] <- test
   
   
}

library(data.table)
resAll <- rbindlist(resList)


resAll$Lineage <- gsub("hereO", "\U002A (Omicron)", resAll$Lineage)
resAll$Lineage <- gsub("hereA", " (Alpha)", resAll$Lineage)
resAll$Lineage <- gsub("hereB", " (Beta)", resAll$Lineage)
resAll$Lineage <- gsub("hereG", "\U002A (Gamma)", resAll$Lineage)
resAll$Lineage <- gsub("hereD2", " (Delta)", resAll$Lineage)
resAll$Lineage <- gsub("hereD", "\U002A (Delta)", resAll$Lineage)


x <- data.frame(tapply(resAll$count, resAll$Lineage, sum))
x$Lineage <- row.names(x)
x <- x[order(x$tapply.resAll.count..resAll.Lineage..sum., decreasing = T),]


resAll$Lineage <-  factor(resAll$Lineage, levels= c(x$Lineage[x$Lineage != "Others"], "Others"))

library(randomcoloR)
n <- 13

library(paletteer)
palette <- paletteer_d("ggsci::springfield_simpsons") 

palette <- palette[c(1,5,10,11,12,2,6,7,9,4,8,13,3)]
 
resAll <- resAll[resAll$count != 0,]

xLables <- unique(resAll$week)
xLables2 <- xLables[c(T,F)]
xLables[!xLables %in% xLables2] <- ""

global_size = 15

g<- ggplot(data=resAll, aes(x=week, y=percentage, fill=Lineage)) +
   geom_bar(stat="identity",position = "fill")+ 
   scale_y_continuous(labels = scales::percent_format())+
   xlab("Calendar week 2021")+ylab("Percentage of total lineages")+
   scale_fill_manual(values=palette)+ scale_x_discrete(labels  = xLables)+  
   theme(text = element_text(size=global_size))
g

##ggsave(g, filename=paste0("Figure Lineage per Week percentage.png"), width=8, height = 5)
#ggsave(g, filename=paste0("Supplementary Figure 3.png"), width=8, height = 5)




# Supplementary Figure ContactTracingIntervalDistribution
# Supplementary Figure 8

curGen <- genGen_suplContactTracingInterValDist 
curGen$classification <- "Rejected"
curGen$classification[curGen$Genetic_Distance2 < 2] <- "Accepted"

curGenContact <- curGen[curGen$Typ == "SurvnetKontaktperson",]

curGenContact$dateDiffabs <- abs(edge2$DatumsDifferenz[match(curGenContact$indexSearch, edge2$indexSearch)])

test <- table(curGenContact$dateDiffabs, curGenContact$classification)
test <- data.frame(test)

test$percent <- 0
test$percent[test$Var2 == "Accepted"] <- round(test$Freq[test$Var2 == "Accepted"] /sum(test$Freq[test$Var2 == "Accepted"])*100)
test$percent[test$Var2 == "Rejected"] <-  round(test$Freq[test$Var2 == "Rejected"] /sum(test$Freq[test$Var2 == "Rejected"])*100)

test$percentsave  <- test$percent

test$percent <- paste0(test$percent, "%")

library(ggplot2)
g <- ggplot(test, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Time between case registration dates [days]")+ylab("Number of contact person pairs")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+
   theme(text = element_text(size=15))

g <- g + facet_grid(. ~ test$Var2, scales="free", shrink=T)+theme(plot.title = element_text(hjust = 0.5))

g

##ggsave(g, filename=paste0("ContactTracingIntervalDistribution A (contact).svg"), width=7.5, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 8 A.png"), width=7.5, height = 4)


## same with symptom start dates where possible

css <- data.frame(caseID= c(edgesNew$from_label,edgesNew$to_label), 
                  symptomeStart = c(edgesNew$from_symptom_start,edgesNew$to_symptom_start))

css <- unique(css)

curGen <- genGen_suplContactTracingInterValDist 
curGen$classification <- "Rejected"
curGen$classification[curGen$Genetic_Distance2 < 2] <- "Accepted"

curGenContact <- curGen[curGen$Typ == "SurvnetKontaktperson",]

curGenContact$sympStartindex1 <- css$symptomeStart[match(curGenContact$index1, css$caseID)]
curGenContact$sympStartindex2 <- css$symptomeStart[match(curGenContact$index2, css$caseID)]

curGenContact <- curGenContact[curGenContact$sympStartindex1 != "" & curGenContact$sympStartindex2 != "",]

curGenContact$sympStartindex1 <- as.character(lapply(curGenContact$sympStartindex1, function(x){
   y <- strsplit(x,"\\." )[[1]]
   as.character(paste(c(y[3],y[2],y[1]), collapse = "-"))
} ))

curGenContact$sympStartindex1 <- as.Date(curGenContact$sympStartindex1)

curGenContact$sympStartindex2 <- as.character(lapply(curGenContact$sympStartindex2, function(x){
   y <- strsplit(x,"\\." )[[1]]
   as.character(paste(c(y[3],y[2],y[1]), collapse = "-"))
} ))

curGenContact$sympStartindex2 <- as.Date(curGenContact$sympStartindex2)

curGenContact$dateDiffabs <- abs(as.numeric(curGenContact$sympStartindex1-curGenContact$sympStartindex2))

test <- table(curGenContact$dateDiffabs, curGenContact$classification)
test <- data.frame(test)

test$percent <- 0
test$percent[test$Var2 == "Accepted"] <- round(test$Freq[test$Var2 == "Accepted"] /sum(test$Freq[test$Var2 == "Accepted"])*100)
test$percent[test$Var2 == "Rejected"] <-  round(test$Freq[test$Var2 == "Rejected"] /sum(test$Freq[test$Var2 == "Rejected"])*100)

test$percentsave  <- test$percent

test$percent <- paste0(test$percent, "%")

library(ggplot2)
g <- ggplot(test, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Time between case symptom start dates [days]")+ylab("Number of contact person pairs")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+
   theme(text = element_text(size=15))

g <- g + facet_grid(. ~ test$Var2, scales="free", shrink=T)+theme(plot.title = element_text(hjust = 0.5))

g

##ggsave(g, filename=paste0("ContactTracingIntervalDistribution A (contact, symptom).svg"), width=8, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 8 B.png"), width=8, height = 4)



##Date differences for infected by
curGenContact <- curGen[curGen$Typ == "SurvnetAngestecktBei",]

curGenContact$dateDiffabs <- edge2$DatumsDifferenz[match(curGenContact$indexSearch, edge2$indexSearch)]

test <- table(curGenContact$dateDiffabs, curGenContact$classification)
test <- data.frame(test)

test$percent <- 0
test$percent[test$Var2 == "Accepted"] <- round(test$Freq[test$Var2 == "Accepted"] /sum(test$Freq[test$Var2 == "Accepted"])*100)
test$percent[test$Var2 == "Rejected"] <-  round(test$Freq[test$Var2 == "Rejected"] /sum(test$Freq[test$Var2 == "Rejected"])*100)

test$percentsave  <- test$percent

test$percent <- paste0(test$percent, "%")

library(ggplot2)
g <- ggplot(test, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Days the index case was registered after the putative infection source case")+ylab("Number of 'Infected by' pairs")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+
   theme(text = element_text(size=15))

g <- g + facet_grid(. ~ test$Var2, scales="free", shrink=T)+theme(plot.title = element_text(hjust = 0.5))

g

##ggsave(g, filename=paste0("ContactTracingIntervalDistribution B (infectedBy).svg"), width=7.5, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 8 C.png"), width=7.5, height = 4)







##Date differences for infected by
curGenContact <- curGen[curGen$Typ == "SurvnetAngestecktBei",]

curGenContact$index1 <- edge2$CaseID_Index[match(curGenContact$indexSearch, edge2$indexSearch)]
curGenContact$index2 <- edge2$CaseID_Kontaktperson[match(curGenContact$indexSearch, edge2$indexSearch)]

curGenContact$sympStartindex1 <- css$symptomeStart[match(curGenContact$index1, css$caseID)]
curGenContact$sympStartindex2 <- css$symptomeStart[match(curGenContact$index2, css$caseID)]

curGenContact <- curGenContact[curGenContact$sympStartindex1 != "" & curGenContact$sympStartindex2 != "",]

curGenContact$sympStartindex1 <- as.character(lapply(curGenContact$sympStartindex1, function(x){
   y <- strsplit(x,"\\." )[[1]]
   as.character(paste(c(y[3],y[2],y[1]), collapse = "-"))
} ))

curGenContact$sympStartindex1 <- as.Date(curGenContact$sympStartindex1)

curGenContact$sympStartindex2 <- as.character(lapply(curGenContact$sympStartindex2, function(x){
   y <- strsplit(x,"\\." )[[1]]
   as.character(paste(c(y[3],y[2],y[1]), collapse = "-"))
} ))

curGenContact$sympStartindex2 <- as.Date(curGenContact$sympStartindex2)

curGenContact$dateDiffabs <- as.numeric(curGenContact$sympStartindex1-curGenContact$sympStartindex2)

test <- table(curGenContact$dateDiffabs, curGenContact$classification)
test <- data.frame(test)

test$percent <- 0
test$percent[test$Var2 == "Accepted"] <- round(test$Freq[test$Var2 == "Accepted"] /sum(test$Freq[test$Var2 == "Accepted"])*100)
test$percent[test$Var2 == "Rejected"] <-  round(test$Freq[test$Var2 == "Rejected"] /sum(test$Freq[test$Var2 == "Rejected"])*100)

test$percentsave  <- test$percent

test$percent <- paste0(test$percent, "%")

library(ggplot2)
g <- ggplot(test, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Days the index case showed symptoms after the putative infection source case")+ylab("Number of 'Infected by' pairs")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+
   theme(text = element_text(size=15))

g <- g + facet_grid(. ~ test$Var2, scales="free", shrink=T)+theme(plot.title = element_text(hjust = 0.5))

g

##ggsave(g, filename=paste0("ContactTracingIntervalDistribution B (infectedBy, symptoms).svg"), width=8, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 8 D.png"), width=7.5, height = 4)




#### ContactTracingIntervalDistribution  SelbeAdresse

curGen <- genGen_suplContactTracingInterValDist 
curGen$classification <- "Rejected"
curGen$classification[curGen$Genetic_Distance2 < 2] <- "Accepted"

curGenContact <- curGen[curGen$Typ == "SelbeAdresse",]

curGenContact$dateDiffabs <- abs(edge2$DatumsDifferenz[match(curGenContact$indexSearch, edge2$indexSearch)])

test <- table(curGenContact$dateDiffabs, curGenContact$classification)
test <- data.frame(test)

test$percent <- 0
test$percent[test$Var2 == "Accepted"] <- round(test$Freq[test$Var2 == "Accepted"] /sum(test$Freq[test$Var2 == "Accepted"])*100)
test$percent[test$Var2 == "Rejected"] <-  round(test$Freq[test$Var2 == "Rejected"] /sum(test$Freq[test$Var2 == "Rejected"])*100)

test$percentsave  <- test$percent

test$percent <- paste0(test$percent, "%")

library(ggplot2)
g <- ggplot(test, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Time between case registration dates")+ylab("Number of 'same address' case pairs")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+
   theme(text = element_text(size=15))

g <- g + facet_grid(. ~ test$Var2, scales="free", shrink=T)+theme(plot.title = element_text(hjust = 0.5))

g

##ggsave(g, filename=paste0("ContactTracingIntervalDistribution E (sameaddress).svg"), width=7.5, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 8 E.png"), width=7.5, height = 4)


## same with symptom start dates where possible

css <- data.frame(caseID= c(edgesNew$from_label,edgesNew$to_label), 
                  symptomeStart = c(edgesNew$from_symptom_start,edgesNew$to_symptom_start))

css <- unique(css)

curGen <- genGen_suplContactTracingInterValDist 
curGen$classification <- "Rejected"
curGen$classification[curGen$Genetic_Distance2 < 2] <- "Accepted"

curGenContact <- curGen[curGen$Typ == "SelbeAdresse",]

curGenContact$sympStartindex1 <- css$symptomeStart[match(curGenContact$index1, css$caseID)]
curGenContact$sympStartindex2 <- css$symptomeStart[match(curGenContact$index2, css$caseID)]

curGenContact <- curGenContact[curGenContact$sympStartindex1 != "" & curGenContact$sympStartindex2 != "",]

curGenContact$sympStartindex1 <- as.character(lapply(curGenContact$sympStartindex1, function(x){
   y <- strsplit(x,"\\." )[[1]]
   as.character(paste(c(y[3],y[2],y[1]), collapse = "-"))
} ))

curGenContact$sympStartindex1 <- as.Date(curGenContact$sympStartindex1)

curGenContact$sympStartindex2 <- as.character(lapply(curGenContact$sympStartindex2, function(x){
   y <- strsplit(x,"\\." )[[1]]
   as.character(paste(c(y[3],y[2],y[1]), collapse = "-"))
} ))

curGenContact$sympStartindex2 <- as.Date(curGenContact$sympStartindex2)

curGenContact$dateDiffabs <- abs(as.numeric(curGenContact$sympStartindex1-curGenContact$sympStartindex2))

test <- table(curGenContact$dateDiffabs, curGenContact$classification)
test <- data.frame(test)

test$Var1 <- as.numeric(as.character(test$Var1))

test$Freq[test$Var1 == 14 & test$Var2 == "Accepted"] <- sum(test$Freq[test$Var1 >= 14 & test$Var2 == "Accepted"])
test$Freq[test$Var1 == 14 & test$Var2 == "Rejected"] <- sum(test$Freq[test$Var1 >= 14 & test$Var2 == "Rejected"])

test <- test[test$Var1 <= 14,]

test$Var1[test$Var1 == 14] <- ">13"

test$Var1 <- factor(test$Var1, levels=c(as.character(0:13), ">13"))

test$percent <- 0
test$percent[test$Var2 == "Accepted"] <- round(test$Freq[test$Var2 == "Accepted"] /sum(test$Freq[test$Var2 == "Accepted"])*100)
test$percent[test$Var2 == "Rejected"] <-  round(test$Freq[test$Var2 == "Rejected"] /sum(test$Freq[test$Var2 == "Rejected"])*100)

test$percentsave  <- test$percent

test$percent <- paste0(test$percent, "%")

library(ggplot2)
g <- ggplot(test, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Time between case symptom start dates")+ylab("Number of 'same address' case pairs")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+
   theme(text = element_text(size=15))

g <- g + facet_grid(. ~ test$Var2, scales="free", shrink=T)+theme(plot.title = element_text(hjust = 0.5))

g

##ggsave(g, filename=paste0("ContactTracingIntervalDistribution F (sameaddress, symptom).svg"), width=10, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 8 F.png"), width=7.5, height = 4)









#### ContactTracingIntervalDistribution  SelbeAdresse / found in contact tracing or not

curGen <- genGen_suplContactTracingInterValDist 
curGen$indexSearch2 <- paste(curGen$index1, curGen$index2)
curGen$classification <- "Rejected"
curGen$classification[curGen$Genetic_Distance2 < 2] <- "Accepted"

curGenContact <- curGen[curGen$Typ == "SelbeAdresse",]
Rest <- curGen[!curGen$Typ %in% c("SelbeAdresse", "SelbeAdresseNachname" ) ,]

#curGenContact <- curGenContact[curGenContact$classification == "Accepted",]

curGenContact$classification2[curGenContact$indexSearch2 %in% Rest$indexSearch2] <- "Contact tracing link"
curGenContact$classification2[!curGenContact$indexSearch2 %in% Rest$indexSearch2] <- "No contact tracing link"

curGenContact$dateDiffabs <- abs(edge2$DatumsDifferenz[match(curGenContact$indexSearch, edge2$indexSearch)])

test <- table(curGenContact$dateDiffabs, curGenContact$classification, curGenContact$classification2)
test <- data.frame(test)

test$percent <- 0
vec_contact_acc <- test$Var3 == "Contact tracing link" & test$Var2 == "Accepted"
vec_contact_rej <- test$Var3 == "Contact tracing link" & test$Var2 == "Rejected"
vec_NoContact_acc <- test$Var3 == "No contact tracing link" & test$Var2 == "Accepted"
vec_NoContact_rej <-test$Var3 == "No contact tracing link" & test$Var2 == "Rejected"

test$percent[vec_contact_acc] <- round(test$Freq[vec_contact_acc] /sum(test$Freq[vec_contact_acc])*100)
test$percent[vec_contact_rej] <- round(test$Freq[vec_contact_rej] /sum(test$Freq[vec_contact_rej])*100)
test$percent[vec_NoContact_acc] <- round(test$Freq[vec_NoContact_acc] /sum(test$Freq[vec_NoContact_acc])*100)
test$percent[vec_NoContact_rej] <- round(test$Freq[vec_NoContact_rej] /sum(test$Freq[vec_NoContact_rej])*100)

test$percentsave  <- test$percent

test$percent <- paste0(test$percent, "%")

library(ggplot2)
library(ggh4x)
g <- ggplot(test, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Time between case registration dates")+ylab("Number of 'same address' case pairs")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+
   theme(text = element_text(size=15))

g <- g + facet_nested(. ~ test$Var2 + test$Var3, scales="free", shrink=T)+theme(plot.title = element_text(hjust = 0.5))

g

##ggsave(g, filename=paste0("ContactTracingIntervalDistribution G (sameaddress, accepted, contactTracing).svg"), width=14, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 8 G.png"), width=14, height = 4)







## same with symptom start dates where possible



curGen <- genGen_suplContactTracingInterValDist 
curGen$indexSearch2 <- paste(curGen$index1, curGen$index2)
curGen$classification <- "Rejected"
curGen$classification[curGen$Genetic_Distance2 < 2] <- "Accepted"

curGenContact <- curGen[curGen$Typ == "SelbeAdresse",]
Rest <- curGen[!curGen$Typ %in% c("SelbeAdresse", "SelbeAdresseNachname" ) ,]

css <- data.frame(caseID= c(edgesNew$from_label,edgesNew$to_label), 
                  symptomeStart = c(edgesNew$from_symptom_start,edgesNew$to_symptom_start))

css <- unique(css)

curGenContact$sympStartindex1 <- css$symptomeStart[match(curGenContact$index1, css$caseID)]
curGenContact$sympStartindex2 <- css$symptomeStart[match(curGenContact$index2, css$caseID)]

curGenContact <- curGenContact[curGenContact$sympStartindex1 != "" & curGenContact$sympStartindex2 != "",]

curGenContact$sympStartindex1 <- as.character(lapply(curGenContact$sympStartindex1, function(x){
   y <- strsplit(x,"\\." )[[1]]
   as.character(paste(c(y[3],y[2],y[1]), collapse = "-"))
} ))

curGenContact$sympStartindex1 <- as.Date(curGenContact$sympStartindex1)

curGenContact$sympStartindex2 <- as.character(lapply(curGenContact$sympStartindex2, function(x){
   y <- strsplit(x,"\\." )[[1]]
   as.character(paste(c(y[3],y[2],y[1]), collapse = "-"))
} ))

curGenContact$sympStartindex2 <- as.Date(curGenContact$sympStartindex2)

curGenContact$dateDiffabs <- abs(as.numeric(curGenContact$sympStartindex1-curGenContact$sympStartindex2))


curGenContact$classification2[curGenContact$indexSearch2 %in% Rest$indexSearch2] <- "Contact tracing link"
curGenContact$classification2[!curGenContact$indexSearch2 %in% Rest$indexSearch2] <- "No contact tracing link"

#curGenContact$dateDiffabs <- abs(edge2$DatumsDifferenz[match(curGenContact$indexSearch, edge2$indexSearch)])

test <- table(curGenContact$dateDiffabs, curGenContact$classification, curGenContact$classification2)
test <- data.frame(test)

test$percent <- 0
vec_contact_acc <- test$Var3 == "Contact tracing link" & test$Var2 == "Accepted"
vec_contact_rej <- test$Var3 == "Contact tracing link" & test$Var2 == "Rejected"
vec_NoContact_acc <- test$Var3 == "No contact tracing link" & test$Var2 == "Accepted"
vec_NoContact_rej <- test$Var3 == "No contact tracing link" & test$Var2 == "Rejected"

test$Var1 <- as.numeric(as.character(test$Var1))

test$Freq[test$Var1 == 14 & vec_contact_acc] <- sum(test$Freq[test$Var1 >= 14 & vec_contact_acc])
test$Freq[test$Var1 == 14 & vec_contact_rej] <- sum(test$Freq[test$Var1 >= 14 & vec_contact_rej])
test$Freq[test$Var1 == 14 & vec_NoContact_acc] <- sum(test$Freq[test$Var1 >= 14 & vec_NoContact_acc])
test$Freq[test$Var1 == 14 & vec_NoContact_rej] <- sum(test$Freq[test$Var1 >= 14 & vec_NoContact_rej])


test <- test[test$Var1 <= 14,]
test$Var1[test$Var1 == 14] <- ">13"

test$Var1 <- factor(test$Var1, levels=c(as.character(0:13), ">13"))

vec_contact_acc <- test$Var3 == "Contact tracing link" & test$Var2 == "Accepted"
vec_contact_rej <- test$Var3 == "Contact tracing link" & test$Var2 == "Rejected"
vec_NoContact_acc <- test$Var3 == "No contact tracing link" & test$Var2 == "Accepted"
vec_NoContact_rej <- test$Var3 == "No contact tracing link" & test$Var2 == "Rejected"

test$percent <- 0
test$percent[vec_contact_acc] <- round(test$Freq[vec_contact_acc] /sum(test$Freq[vec_contact_acc])*100)
test$percent[vec_contact_rej] <- round(test$Freq[vec_contact_rej] /sum(test$Freq[vec_contact_rej])*100)
test$percent[vec_NoContact_acc] <- round(test$Freq[vec_NoContact_acc] /sum(test$Freq[vec_NoContact_acc])*100)
test$percent[vec_NoContact_rej] <- round(test$Freq[vec_NoContact_rej] /sum(test$Freq[vec_NoContact_rej])*100)

test$percentsave  <- test$percent

test$percent <- paste0(test$percent, "%")

library(ggplot2)
library(ggh4x)
g <- ggplot(test, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Time between case symptom start dates")+ylab("Number of 'same address' case pairs")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+
   theme(text = element_text(size=15))

g <- g + facet_nested(. ~ test$Var2 + test$Var3, scales="free", shrink=T)+theme(plot.title = element_text(hjust = 0.5))

g

##ggsave(g, filename=paste0("ContactTracingIntervalDistribution H (sameaddress, accepted, contactTracing).svg"), width=14, height = 4)
#ggsave(g, filename=paste0("Supplementary Figure 8 H.png"), width=14, height = 4)



## How many duesseldorf contacts became case?
test <- edge[edge$CaseID_Index %in% nodes$CaseID & edge$Typ == "SurvnetKontaktperson",]
nrow(test)




## Only same address accepted, that are not in contact tracing
## check who are sameName or not

curGen <- genGen_suplContactTracingInterValDist 
curGen$indexSearch2 <- paste(curGen$index1, curGen$index2)
curGen$classification <- "Rejected"
curGen$classification[curGen$Genetic_Distance2 < 2] <- "Accepted"

curGen <- curGen[curGen$classification == "Accepted",]

curGenContact <- curGen[curGen$Typ == "SelbeAdresse",]
Rest <- curGen[!curGen$Typ %in% c("SelbeAdresse", "SelbeAdresseNachname" ) ,]
RestSameName <- curGen[curGen$Typ =="SelbeAdresseNachname" ,]

curGenContact$classification2[curGenContact$indexSearch2 %in% Rest$indexSearch2] <- "Contact tracing link"
curGenContact$classification2[!curGenContact$indexSearch2 %in% Rest$indexSearch2] <- "No contact tracing link"

curGenContact$classification[curGenContact$indexSearch2 %in% RestSameName$indexSearch2] <- "Same Name"
curGenContact$classification[!curGenContact$indexSearch2 %in% RestSameName$indexSearch2] <- "Not same name"

curGenContact$dateDiffabs <- abs(edge2$DatumsDifferenz[match(curGenContact$indexSearch, edge2$indexSearch)])

test <- table(curGenContact$dateDiffabs, curGenContact$classification, curGenContact$classification2)
test <- data.frame(test)

test$percent <- 0
vec_contact_acc <- test$Var3 == "Contact tracing link" & test$Var2 == "Same Name"
vec_contact_rej <- test$Var3 == "Contact tracing link" & test$Var2 == "Not same name"
vec_NoContact_acc <- test$Var3 == "No contact tracing link" & test$Var2 == "Same Name"
vec_NoContact_rej <- test$Var3 == "No contact tracing link" & test$Var2 == "Not same name"

test$percent[vec_contact_acc] <- round(test$Freq[vec_contact_acc] /sum(test$Freq[vec_contact_acc])*100)
test$percent[vec_contact_rej] <- round(test$Freq[vec_contact_rej] /sum(test$Freq[vec_contact_rej])*100)
test$percent[vec_NoContact_acc] <- round(test$Freq[vec_NoContact_acc] /sum(test$Freq[vec_NoContact_acc])*100)
test$percent[vec_NoContact_rej] <- round(test$Freq[vec_NoContact_rej] /sum(test$Freq[vec_NoContact_rej])*100)

test$percentsave  <- test$percent

test$percent <- paste0(test$percent, "%")

library(ggplot2)
library(ggh4x)
g <- ggplot(test, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Time between case registration dates")+ylab("Number of accepted 'same address' case pairs")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+
   theme(text = element_text(size=15))

g <- g + facet_nested(. ~ test$Var2 + test$Var3, scales="free", shrink=T)+theme(plot.title = element_text(hjust = 0.5))

g

##ggsave(g, filename=paste0("ContactTracingIntervalDistribution I (sameaddress, accepted, contactTracing, lastname).svg"), width=14, height = 4.5)
#ggsave(g, filename=paste0("Supplementary Figure 8 I.png"), width=14, height = 4.5)






## same with symptom start dates where possible


curGen <- genGen_suplContactTracingInterValDist 
curGen$indexSearch2 <- paste(curGen$index1, curGen$index2)
curGen$classification <- "Rejected"
curGen$classification[curGen$Genetic_Distance2 < 2] <- "Accepted"

curGen <- curGen[curGen$classification == "Accepted",]

curGenContact <- curGen[curGen$Typ == "SelbeAdresse",]
Rest <- curGen[!curGen$Typ %in% c("SelbeAdresse", "SelbeAdresseNachname" ) ,]
RestSameName <- curGen[curGen$Typ =="SelbeAdresseNachname" ,]

css <- data.frame(caseID= c(edgesNew$from_label,edgesNew$to_label), 
                  symptomeStart = c(edgesNew$from_symptom_start,edgesNew$to_symptom_start))

css <- unique(css)

curGenContact$sympStartindex1 <- css$symptomeStart[match(curGenContact$index1, css$caseID)]
curGenContact$sympStartindex2 <- css$symptomeStart[match(curGenContact$index2, css$caseID)]

curGenContact <- curGenContact[curGenContact$sympStartindex1 != "" & curGenContact$sympStartindex2 != "",]

curGenContact$sympStartindex1 <- as.character(lapply(curGenContact$sympStartindex1, function(x){
   y <- strsplit(x,"\\." )[[1]]
   as.character(paste(c(y[3],y[2],y[1]), collapse = "-"))
} ))

curGenContact$sympStartindex1 <- as.Date(curGenContact$sympStartindex1)

curGenContact$sympStartindex2 <- as.character(lapply(curGenContact$sympStartindex2, function(x){
   y <- strsplit(x,"\\." )[[1]]
   as.character(paste(c(y[3],y[2],y[1]), collapse = "-"))
} ))

curGenContact$sympStartindex2 <- as.Date(curGenContact$sympStartindex2)

curGenContact$dateDiffabs <- abs(as.numeric(curGenContact$sympStartindex1-curGenContact$sympStartindex2))

curGenContact$classification2[curGenContact$indexSearch2 %in% Rest$indexSearch2] <- "Contact tracing link"
curGenContact$classification2[!curGenContact$indexSearch2 %in% Rest$indexSearch2] <- "No contact tracing link"

curGenContact$classification[curGenContact$indexSearch2 %in% RestSameName$indexSearch2] <- "Same Name"
curGenContact$classification[!curGenContact$indexSearch2 %in% RestSameName$indexSearch2] <- "Not same name"

#curGenContact$dateDiffabs <- abs(edge2$DatumsDifferenz[match(curGenContact$indexSearch, edge2$indexSearch)])

test <- table(curGenContact$dateDiffabs, curGenContact$classification, curGenContact$classification2)
test <- data.frame(test)

test$percent <- 0
vec_contact_acc <- test$Var3 == "Contact tracing link" & test$Var2 == "Same Name"
vec_contact_rej <- test$Var3 == "Contact tracing link" & test$Var2 == "Not same name"
vec_NoContact_acc <- test$Var3 == "No contact tracing link" & test$Var2 == "Same Name"
vec_NoContact_rej <- test$Var3 == "No contact tracing link" & test$Var2 == "Not same name"

test$Var1 <- as.numeric(as.character(test$Var1))

test$Freq[test$Var1 == 14 & vec_contact_acc] <- sum(test$Freq[test$Var1 >= 14 & vec_contact_acc])
test$Freq[test$Var1 == 14 & vec_contact_rej] <- sum(test$Freq[test$Var1 >= 14 & vec_contact_rej])
test$Freq[test$Var1 == 14 & vec_NoContact_acc] <- sum(test$Freq[test$Var1 >= 14 & vec_NoContact_acc])
test$Freq[test$Var1 == 14 & vec_NoContact_rej] <- sum(test$Freq[test$Var1 >= 14 & vec_NoContact_rej])


test <- test[test$Var1 <= 14,]

test$Var1[test$Var1 == 14] <- ">13"
test$Var1 <- factor(test$Var1, levels=c(as.character(0:13), ">13"))

vec_contact_acc <- test$Var3 == "Contact tracing link" & test$Var2 == "Same Name"
vec_contact_rej <- test$Var3 == "Contact tracing link" & test$Var2 == "Not same name"
vec_NoContact_acc <- test$Var3 == "No contact tracing link" & test$Var2 == "Same Name"
vec_NoContact_rej <- test$Var3 == "No contact tracing link" & test$Var2 == "Not same name"


test$percent[vec_contact_acc] <- round(test$Freq[vec_contact_acc] /sum(test$Freq[vec_contact_acc])*100)
test$percent[vec_contact_rej] <- round(test$Freq[vec_contact_rej] /sum(test$Freq[vec_contact_rej])*100)
test$percent[vec_NoContact_acc] <- round(test$Freq[vec_NoContact_acc] /sum(test$Freq[vec_NoContact_acc])*100)
test$percent[vec_NoContact_rej] <- round(test$Freq[vec_NoContact_rej] /sum(test$Freq[vec_NoContact_rej])*100)

test$percentsave  <- test$percent

test$percent <- paste0(test$percent, "%")

library(ggplot2)
library(ggh4x)
g <- ggplot(test, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Time between case symptom start dates")+ylab("Number of accepted 'same address' case pairs")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+
   theme(text = element_text(size=15))

g <- g + facet_nested(. ~ test$Var2 + test$Var3, scales="free", shrink=T)+theme(plot.title = element_text(hjust = 0.5))

g

##ggsave(g, filename=paste0("ContactTracingIntervalDistribution J (sameaddress, accepted, contactTracing sameName symptom).svg"), width=14, height = 4.5)
#ggsave(g, filename=paste0("Supplementary Figure 8 J.png"), width=14, height = 4.5)





## suppl table ContactTracingConnections
## Supplementary Figure 4
# o	a) number of registered total contacts per case (independent of whether they became positive or not)

totalCaseContacts <- nodes$Count_Kontaktpersonen
totalCaseContacts[totalCaseContacts >= 10] <- ">= 10"
totalCaseContacts <- data.frame(total=totalCaseContacts)
totalCaseContacts$total <- factor(totalCaseContacts$total, levels=c(as.character(c(0:9, ">= 10"))))

totalCaseContactsHist <- data.frame(table(totalCaseContacts$total))
totalCaseContactsHist$percent <- round(100*totalCaseContactsHist$Freq/sum(totalCaseContactsHist$Freq),1)
totalCaseContactsHist$percent  <- paste0(totalCaseContactsHist$percent, "%")


library(ggplot2)
g <- ggplot(totalCaseContactsHist, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Number of registered contact persons")+ylab("Number of cases")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")

##ggsave(g, file="ContactTracingConnections A.svg", width=5, height = 3)
#ggsave(g, file="Supplementary Figure 4 A.svg", width=5, height = 3)



# o	b) number of registered DÜsseldorf contacts per case (independent of whether they became positive or not);

totalCaseContacts <- nodes$Count_Kontaktpersonen_Duesseldorf
totalCaseContacts[totalCaseContacts >= 10] <- ">= 10"
totalCaseContacts <- data.frame(total=totalCaseContacts)
totalCaseContacts$total <- factor(totalCaseContacts$total, levels=c(as.character(c(0:9, ">= 10"))))

totalCaseContactsHist <- data.frame(table(totalCaseContacts$total))
totalCaseContactsHist$percent <- round(100*totalCaseContactsHist$Freq/sum(totalCaseContactsHist$Freq),1)
totalCaseContactsHist$percent  <- paste0(totalCaseContactsHist$percent, "%")


library(ggplot2)
g <- ggplot(totalCaseContactsHist, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Number of registered contact persons in DÜsseldorf")+ylab("Number of cases")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")

##ggsave(g, file="ContactTracingConnections B.svg", width=5, height = 3)
#ggsave(g, file="Supplementary Figure 4 B.svg", width=5, height = 3)

# o	c) total contact edges in the integrated graph per DÜsseldorf case
contacts  <- edge2[edge2$Typ == "SurvnetKontaktperson",]
contacts <- contacts[contacts$indexSearch %in% edgesGraph$indexSearch,]
contacts <- contacts[contacts$DatumsDifferenz <= 14 ,]

#contacts <- contacts[abs(contacts$DatumsDifferenz) <= 14,]
contacts$c <- 1

totalCaseContacts <- tapply(contacts$c, contacts$CaseID_Index, sum)
totalCaseContacts[totalCaseContacts >= 6] <- ">= 6"
totalCaseContacts <- data.frame(total=totalCaseContacts)
totalCaseContacts$total <- factor(totalCaseContacts$total, levels=c(as.character(c(0:5, ">= 6"))))

totalCaseContactsHist <- data.frame(table(totalCaseContacts$total))
totalCaseContactsHist$Freq[totalCaseContactsHist$Var1 == "0"] <- sum(!nodes$CaseID %in% contacts$CaseID_Index)
totalCaseContactsHist$percent <- round(100*totalCaseContactsHist$Freq/sum(totalCaseContactsHist$Freq),1)
totalCaseContactsHist$percent  <- paste0(totalCaseContactsHist$percent, "%")


library(ggplot2)
g <- ggplot(totalCaseContactsHist, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Contact edges per case in the integrated graph")+ylab("Number of cases")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")

##ggsave(g, file="Suppl Fig ContactTracingConnections C.svg", width=4, height = 3)
#ggsave(g, file="Supplementary Figure 4 C.svg", width=4, height = 3)


# o	d) outgoing and incoming case-case edges of type "contact" (after directionalisation);
g <-read.graph("graph_final_directed_3.gml",format=c("gml"))
edgesDirected <- as_long_data_frame(g)
edgesGraphDirected <- data.frame(         CaseID_Index= edgesDirected$from_label,
                                          CaseID_Kontaktperson= edgesDirected$to_label,
                                          Typ= edgesDirected$type)

contacts <- edgesGraphDirected[edgesGraphDirected$CaseID_Index %in% nodes$CaseID,]

contacts <- contacts[contacts$Typ == "SurvnetKontaktperson",]
contacts$c <- 1

totalCaseContacts <- tapply(contacts$c, contacts$CaseID_Index, sum)
totalCaseContacts[totalCaseContacts >= 6] <- ">= 6"
totalCaseContacts <- data.frame(total=totalCaseContacts)
totalCaseContacts$total <- factor(totalCaseContacts$total, levels=c(as.character(c(0:5, ">= 6"))))


totalCaseContactsHist <- data.frame(table(totalCaseContacts$total))
totalCaseContactsHist$Freq[totalCaseContactsHist$Var1 == "0"] <- sum(!nodes$CaseID %in% contacts$CaseID_Index)
totalCaseContactsHist$percent <- round(100*totalCaseContactsHist$Freq/sum(totalCaseContactsHist$Freq),1)
totalCaseContactsHist$percent  <- paste0(totalCaseContactsHist$percent, "%")
totalCaseContactsHist$type <- "Outgoing"

totalCaseContactsHistout <- totalCaseContactsHist



##Incoming
contacts <- edgesGraphDirected[edgesGraphDirected$CaseID_Kontaktperson %in% nodes$CaseID,]
contacts <- contacts[contacts$Typ == "SurvnetKontaktperson",]
contacts$c <- 1

totalCaseContacts <- tapply(contacts$c, contacts$CaseID_Kontaktperson, sum)
totalCaseContacts[totalCaseContacts >= 4] <- ">= 4"
totalCaseContacts <- data.frame(total=totalCaseContacts)
totalCaseContacts$total <- factor(totalCaseContacts$total, levels=c(as.character(c(0:3, ">= 4"))))

totalCaseContactsHist <- data.frame(table(totalCaseContacts$total))
totalCaseContactsHist$Freq[totalCaseContactsHist$Var1 == "0"] <- sum(!nodes$CaseID %in% contacts$CaseID_Kontaktperson)
totalCaseContactsHist$percent <- round(100*totalCaseContactsHist$Freq/sum(totalCaseContactsHist$Freq),1)
totalCaseContactsHist$percent  <- paste0(totalCaseContactsHist$percent, "%")
totalCaseContactsHist$type <- "Incoming"

all <- rbind(totalCaseContactsHistout, totalCaseContactsHist)

all$type <- factor(all$type, levels=c("Outgoing" , "Incoming"))

library(ggplot2)
g <- ggplot(all, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Directed contact tracing edges")+ylab("Number of cases")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")

g <- g + facet_grid(. ~ all$type, scales="free", space="free")+theme(plot.title = element_text(hjust = 0.5))

g

##ggsave(g, file="Suppl Fig ContactTracingConnections D.svg", width=5, height = 4)
#ggsave(g, file="Supplementary Figure 4 D.svg", width=5, height = 3)





# o	e)  outgoing and incoming case-case edges of type "infectedby" per case
contacts  <- edge2[edge2$Typ == "SurvnetAngestecktBei",]
contacts <- contacts[contacts$indexSearch %in% edgesGraph$indexSearch,]
contacts <- contacts[contacts$DatumsDifferenz <= 14 ,]


#contacts <- contacts[abs(contacts$DatumsDifferenz) <= 14,]
contacts$c <- 1

totalCaseContacts <- tapply(contacts$c, contacts$CaseID_Index, sum)
totalCaseContacts <- data.frame(total=totalCaseContacts)
totalCaseContacts$total <- factor(totalCaseContacts$total, levels=c(as.character(c(0:1))))

totalCaseContactsHist <- data.frame(table(totalCaseContacts$total))
totalCaseContactsHist$Freq[totalCaseContactsHist$Var1 == "0"] <- sum(!nodes$CaseID %in% contacts$CaseID_Index)
totalCaseContactsHist$percent <- round(100*totalCaseContactsHist$Freq/sum(totalCaseContactsHist$Freq),1)
totalCaseContactsHist$percent  <- paste0(totalCaseContactsHist$percent, "%")
totalCaseContactsHist$type <- "Outgoing"

totalCaseContactsHistout <- totalCaseContactsHist

contacts  <- edge2[edge2$Typ == "SurvnetAngestecktBei",]
contacts <- contacts[contacts$indexSearch %in% edgesGraph$indexSearch,]
contacts <- contacts[contacts$DatumsDifferenz <= 14 ,]

contacts$c <- 1

totalCaseContacts <- tapply(contacts$c, contacts$CaseID_Kontaktperson, sum)
totalCaseContacts[totalCaseContacts >= 2] <- ">= 2"
totalCaseContacts <- data.frame(total=totalCaseContacts)
totalCaseContacts$total <- factor(totalCaseContacts$total, levels=c(as.character(c(0:1, ">= 2"))))

totalCaseContactsHist <- data.frame(table(totalCaseContacts$total))
totalCaseContactsHist$Freq[totalCaseContactsHist$Var1 == "0"] <- sum(!nodes$CaseID %in% contacts$CaseID_Kontaktperson)
totalCaseContactsHist$percent <- round(100*totalCaseContactsHist$Freq/sum(totalCaseContactsHist$Freq),1)
totalCaseContactsHist$percent  <- paste0(totalCaseContactsHist$percent, "%")
totalCaseContactsHist$type <- "Incoming"

all <- rbind(totalCaseContactsHistout, totalCaseContactsHist)

all$type <- factor(all$type, levels=c("Outgoing" , "Incoming"))

library(ggplot2)
g <- ggplot(all, aes(Var1, Freq ))+ geom_bar(stat="identity",position = "identity")+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Number of InfectedBy connections")+ylab("Number of cases")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")

g <- g + facet_grid(. ~ all$type, scales="free", space="free")+theme(plot.title = element_text(hjust = 0.5))

g

##ggsave(g, file="Suppl Fig ContactTracingConnections E.svg", width=4, height = 4)
#ggsave(g, file="Supplementary Figure 4 E.svg", width=4, height = 3)




## ContactTracingContactsOverTime
## average number of contacts over time
## Supplementary Figure 5

nodes$c <- 0
nodes$c[nodes$Count_Kontaktpersonen > 0] <- 1

nodes$month <- strftime(nodes$Meldedatum2, format = "%m")
geneticsPerMonth <- data.frame(tapply(nodes$c, nodes$month, sum))
geneticsPerMonth$date <- row.names(geneticsPerMonth)
geneticsPerMonth$month <- geneticsPerMonth$date

geneticsPerMonth <- geneticsPerMonth[order(geneticsPerMonth$month, decreasing = F),]
monthFactor <- factor(geneticsPerMonth$month, levels=geneticsPerMonth$month)

geneticsPerMonth$monthfactor <- monthFactor

names(geneticsPerMonth) <- c("traced_cases","date", "month", "monthfactor")

sequenced <- geneticsPerMonth

nodes$month <- strftime(nodes$Meldedatum2, format = "%m")
nodes$c <- 1
geneticsPerMonth <- data.frame(tapply(nodes$c, nodes$month, sum))
geneticsPerMonth$date <- row.names(geneticsPerMonth)
geneticsPerMonth$month <- geneticsPerMonth$date

geneticsPerMonth <- geneticsPerMonth[order( geneticsPerMonth$month, decreasing = F),]
monthFactor <- factor(geneticsPerMonth$month, levels=geneticsPerMonth$month)

geneticsPerMonth$monthfactor <- monthFactor

names(geneticsPerMonth) <- c("traced_cases", "date", "month", "monthfactor")

cases <- geneticsPerMonth


resC <- data.frame(month=cases$monthfactor, count=cases$traced_cases, 
                   type="all reported cases")
resS <- data.frame(month=sequenced$monthfactor, count=sequenced$traced_cases, 
                   type="contact traced cases")

res <- rbind(resC, resS)

res$test <- c(resC$count,resC$count)
res$percent <- round(res$count/res$test*100)
res$percent[res$percent == 100.0] <- ""
res$percent[res$type != "all reported cases"]  <- paste0(res$percent[res$type != "all reported cases"] , "%")

res$type[res$type=="all reported cases"] <- "All reported cases"
res$type[res$type=="contact traced cases"] <- "Cases with contact tracing data"

library(ggplot2)
g<- ggplot(data=res, aes(x=month, y=count, fill=type)) +
   geom_bar(stat="identity",position = "identity")+ 
   #theme(axis.text.x = element_text(angle = 45, hjust=1))+
   geom_text(aes(label = percent), vjust = -0.2, size=3)+
   xlab("Month 2021")+ylab("Number of cases")+theme(legend.title=element_blank())+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=15))
g


## #ggsave(g, file="ContactTracingContactsOverTime A total.svg", width=7, height = 4)
#ggsave(g, file="Supplementary Figure 5 A.svg", width=7, height = 4)






##monthly tracing rate was...
x <- res$percent[res$percent != ""]
x  <- gsub("%", "", x)
mean(as.numeric(x ))


## average over time (> 0 only)

nodes$c <- 0
nodes$c[nodes$Count_Kontaktpersonen > 0] <- 1

nodes$month <- strftime(nodes$Meldedatum2, format = "%m")

contacts <- data.frame(contacts=nodes$Count_Kontaktpersonen, Month=nodes$month)
contacts <- contacts[contacts$contacts >0,]

## Boxplot
g <- ggplot(contacts, aes(x=Month, y=contacts)) + 
   geom_boxplot()+ylab("Number of registered contacts")+xlab("Month 2021")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=15))

g <- g +
   coord_cartesian(ylim=c(0, 50)) 


###ggsave(g, file="Suppl Fig ContactTracingContactsOverTime B Box.svg", width=5, height = 5)
#ggsave(g, file="Supplementary Figure 5 B.svg", width=7, height = 4)


## Zoome in
g <- ggplot(contacts, aes(x=Month, y=contacts)) + 
   geom_boxplot()+ylab("Number of registered contacts")+xlab("Month 2021")+
   scale_fill_brewer(palette="Set2")+theme(text = element_text(size=15))+
   coord_cartesian(ylim=c(0, 10)) 

g


###ggsave(g, file="Suppl Fig ContactTracingContactsOverTime B Box zoomeIn.svg", width=5, height = 5)
#ggsave(g, file="Supplementary Figure 5 C.svg", width=7, height = 4)



## Singletons over time per number or Proportion of sequenced cases

nodes2$month <- strftime(nodes2$Meldedatum2, format = "%V")
nodes2$count <- 1

i <- 1
nodes2$n_genRelatedCases <- 0
nodes2$minGenDistance <- 0
nodes2$relatedSeqs <- ""


for(i in 1:nrow(nodes2)){
   
   curSeqID <- nodes2$SeqID[i]
   
   cc <- expand.grid(curSeqID, nodes2$SeqID[!nodes2$SeqID %in% curSeqID])

   cc$Var1 <- as.character(cc$Var1)
   cc$Var2 <- as.character(cc$Var2)
    
   cc$dist <- as.numeric(lapply(1:nrow(cc), function(x){
      
      getGeneticDistance(cc$Var1[x], cc$Var2[x])
      
   }))
   
   ## Number of community cases with low genetic distance
   acc <- sum(cc$dist < 2)
   lowestDist <- min(cc$dist)
   
   nodes2$n_genRelatedCases[i] <- acc
   nodes2$minGenDistance[i] <- lowestDist
   nodes2$relatedSeqs[i] <- paste0(cc$Var2[cc$dist < 2], collapse=",")

}

save(nodes2, file="nodes2_singletons")

nodes4 <- nodes2[nodes2$n_genRelatedCases == 0,]
hist(nodes4$minGenDistance, breaks=40, xlim=c(0,20))


library(ggplot2)


library(RColorBrewer)
set2_colors <- brewer.pal(3, "Set2")
set2_colors

gg <- ggplot(nodes4, aes(x = minGenDistance)) +
   geom_histogram(binwidth = 1, fill = "#66C2A5", color = "black") +
   labs( 
      x = "Minimum Genetic Distance",
      y = "Number of cases"
   )+
   theme(text = element_text(size=15))

#ggsave(gg, filename="Minimum genetic distance of singletons.svg", width=7, height = 5 )


# Singleton
# - Plot über Zeit (als Anteil der sequenzierten Cases?)
  
pd <- data.frame(tapply(nodes2$count, nodes2$month, sum))
pd$week <- row.names(pd)
names(pd) <- c("sequenced", "week")
row.names(pd)<- NULL

nodes2$count2 <- nodes2$count
nodes2$count2[nodes2$n_genRelatedCases > 0] <- 0


pd2 <- data.frame(tapply(nodes2$count2, nodes2$month, sum))
pd2$week <- row.names(pd2)
names(pd2) <- c("sequenced", "week")
row.names(pd2)<- NULL


pd$singletons <- pd2$sequenced[match(pd$week, pd2$week)]
pd$percentage_singletons <- round(pd$singletons/pd$sequenced*100,1)

head(pd)


gg <- ggplot(pd, aes(x = week)) +
   geom_col(aes(y = singletons), fill = "#66C2A5") +
   geom_point(aes(y = percentage_singletons * max(singletons) / max(percentage_singletons)),
              color = "black", size = 2) +
   geom_line(aes(y = percentage_singletons * max(singletons) / max(percentage_singletons), group = 1),
             color = "black", linewidth = 1) +
   scale_y_continuous(
      name = "Number of singletons",
      sec.axis = sec_axis(~ . * max(pd$percentage_singletons) / max(pd$singletons),
                          name = "Number of singletons per sequenced cases [%]")
   )  
gg
#ggsave(gg, filename="Number and percentage of singletons.svg", width=10, height = 4 )

mean(pd$percentage_singletons)


# - Viereldertafel genetische Singletons v/s "Kontakt-Singletons"
# o eingeengt auf sequenzierte Cases
# o alle Kontakt-Typen oder nur Forward Contact Tracing (ggf. einmal beides anschauen)
nodes2$n_epi   <- 0
nodes2$n_cont  <- 0

i <- 1
for(i in 1:nrow(nodes2)){
   
   curID <- nodes2$CaseID[i]
 
   curEdges <- edge2[(edge2$CaseID_Index == curID  ) |
                     (edge2$CaseID_Kontaktperson == curID ),  ]
   
   n_epi  <- nrow(curEdges)
   n_cont <- nrow(curEdges[curEdges$Typ == "SurvnetKontaktperson",])
   
   nodes2$n_epi[i]  <- n_epi
   nodes2$n_cont[i] <- n_cont
}

#                               Epidemiolically related cases       No Epidemiolically related cases
# Genetically related cases            4931                                  1370
# No genetically related cases          949                                   778

sum(nodes2$count2 == 0 & nodes2$n_epi > 0)

sum(nodes2$count2 == 1 & nodes2$n_epi > 0)

sum(nodes2$count2 == 0 & nodes2$n_epi == 0)

sum(nodes2$count2 == 1 & nodes2$n_epi == 0)



sum(nodes2$count2 == 0 & nodes2$n_cont > 0)

sum(nodes2$count2 == 1 & nodes2$n_cont > 0)

sum(nodes2$count2 == 0 & nodes2$n_cont == 0)

sum(nodes2$count2 == 1 & nodes2$n_cont == 0)


#                               Epidemiolically related cases       No Epidemiolically related cases
# Genetically related cases            4904                                   1349
# No genetically related cases          976     


##Check new genetic distances 

dm <- read.delim("/mock/Mock_DM.csv")
head(dm)



### genetically linked cases per context case

KPpos_classification$Aktenzeichen <- gsub("[\t\r\n]", "", KPpos_classification$Aktenzeichen)
KPpos_classification$contect_CaseID <- ""

i <- 1
for(i in 1:nrow(KPpos_classification)){
   
   kpse <- edge2
   kpse <- kpse[kpse$Kontaktperson_Ort %in% KPpos_classification$Aktenzeichen[i],]
   idx <- as.character(KPpos_classification$IndexFall_Token[i])
   if(nrow(kpse) != 0){
      kpse$uniqueID <- paste0(kpse$index1, ",", kpse$index2)
      kpse$dups <- duplicated(kpse$uniqueID)
      kpse <- kpse[kpse$dups == F,]
      ids <- c(kpse$CaseID_Index, kpse$CaseID_Kontaktperson)
      KPpos_classification$contect_CaseID[i] <- ids[ids %in% idx]
      
   }
}


## create context CaseID lists
contextList <- data.frame(context=unique(KPpos_classification$context), associatedCases="",
                          n_cases=0, n_sequencedCases=0, n_genLinkedCases =0,
                          n_genLinkedCases_unique=0, n_contactCases=0)
load("nodes2_singletons")

i <- 1
for(i in 1:nrow(contextList)){
   
   ctx <- contextList$context[i]
   subSet <- KPpos_classification[KPpos_classification$context == ctx,]
   idxs <- as.character(subSet$IndexFall_Token)
   idxsKP <- as.character(subSet$contect_CaseID)
   idxsKP <- idxsKP[idxsKP != ""]
   
   ids <- c(idxs, idxsKP)
   ids <- unique(ids)
   
   contextList$associatedCases[i] <- paste0(ids, collapse = ",")
   
   ## Get number of cases
   contextList$n_cases[i] <- length(ids)
   ids_sequenced <- ids[ids %in% nodes2$CaseID]
   
   ## Get number of sequenced cases
   contextList$n_sequencedCases[i] <- length(ids_sequenced)
   
   ## Get number of genetically linked cases to sequenced cases
   ids_sequenced_genLinked <- nodes2$relatedSeqs[nodes2$CaseID %in% ids_sequenced]
   ids_sequenced_genLinked <- ids_sequenced_genLinked[ids_sequenced_genLinked != ""]
   ids_sequenced_genLinked <- paste(ids_sequenced_genLinked, collapse = ",")
   ids_sequenced_genLinked <- strsplit(ids_sequenced_genLinked, ",")[[1]]
   ids_sequenced_genLinked <- unique(ids_sequenced_genLinked)
   
   contextList$n_genLinkedCases_unique[i] <- length(ids_sequenced_genLinked)
   contextList$n_genLinkedCases[i] <- sum(nodes2$n_genRelatedCases[nodes2$CaseID %in% ids_sequenced])
   
   ## Get number of contact cases
   curContacts <- edge2[edge2$CaseID_Index %in% ids & edge2$Typ == "SurvnetKontaktperson",]
   curContacts <- unique(curContacts$CaseID_Kontaktperson)
   
   contextList$n_contactCases[i] <- length(curContacts)
   
}


contextList$linked_perSequenced_unique <- round(contextList$n_genLinkedCases_unique / contextList$n_contactCases,1)


rate <- contextList
rate$Context <- rate$context

rate$Context[rate$Context =="Private Houshold"] <- "Private\nHoushold"
rate$Context[rate$Context =="Recreational context"] <- "Recreational\nContext"
rate$Context[rate$Context =="Kindergarten/Daycare"] <- "Kindergarten/ \nDaycare"
rate$Context[rate$Context =="Residential Care Facility"] <- "Residential\nCare Facility"


library(ggplot2)
library(ggbreak)
library(scales)

library(dplyr)
library(purrr)

contextList <- contextList %>%
   mutate(
      rate = n_genLinkedCases_unique / n_contactCases,  # events per sequenced case
      ci = map2(n_genLinkedCases_unique, n_contactCases, ~ {
         if (.y > 0) {
            # Poisson CI for count, then divide by exposure
            poisson.test(.x, T = .y)$conf.int
         } else {
            c(NA, NA)
         }
      }),
      rateLow = map_dbl(ci, 1),
      rateHigh = map_dbl(ci, 2)
   ) %>%
   select(-ci) %>%
   mutate(
      linkedPerc = rate ,
      linkedLow  = rateLow ,
      linkedHigh = rateHigh
   )


max_count <- max(contextList$n_genLinkedCases_unique, na.rm = TRUE)
max_percent <- max(contextList$linkedPerc, na.rm = TRUE)
scale_factor <- max_count / max_percent

library(ggplot2)

contextList <- contextList %>%
   arrange(desc(n_genLinkedCases_unique)) %>%
   mutate(context = factor(context, levels = context))


g2 <- ggplot(contextList, aes(x=context)) +
   geom_bar(aes(y=n_genLinkedCases_unique, fill="Number of genetically linked cases"), stat="identity") +
   geom_point(aes(y=linkedPerc * scale_factor, color="Linked cases per contact case"), size=2) +
   geom_errorbar(aes(ymin=linkedLow * scale_factor, ymax=linkedHigh * scale_factor,
                     color="Linked cases per contact case"), width=0.2) +
   scale_y_continuous(
      name = "Number of genetically linked cases",
      sec.axis = sec_axis(~ . / scale_factor, name="Linked cases per contact case")
   ) +
   scale_fill_manual(values=c("Number of genetically linked cases"="skyblue")) +
   scale_color_manual(values=c("Linked cases per contact case"="black")) +
   theme_minimal(base_size = 15) +
   theme(
      axis.text.x = element_text(size=12, angle=45, hjust=1),
      legend.title = element_blank(),
      legend.position = "top"
   )+xlab("")
g2
#ggsave(g2, filename="Unique linked Cases per contact case context.svg", width = 10, height = 5)



# # Wenn wir die sequenzierten Cases, die keine Contact Tracing-Verbindungen haben,
# danach stratifizieren, ob sie genetische Links haben, würden wir erwarten:
# #    für Fälle mit genetischen Links: wahrscheinlich eine erhöhte Rate von 
#    "noncompliant" und vielleicht auch "nicht erreicht"
# # für Fälle ohne genetische Links: wahrscheinlich eine erhöhte Rate von "wirklich keine Kontakte"
# # Lutz sagt, dass man "noncompliant" und "nicht erreicht" wahrscheinlich für
# ein Random Sample herausfinden könnte, wenn wir ihm die SurvNet-Case-IDs geben können.
nodes3 <- nodes
nodes2$contacts <- 0
nodes3$contacts <- 0

KPpos_classification$IndexFall_Token <- gsub("[\t\r\n]", "", KPpos_classification$IndexFall_Token)
nodes2$contacts[nodes2$CaseID %in% KPpos_classification$IndexFall_Token] <- 1
nodes3$contacts[nodes3$CaseID %in% KPpos_classification$IndexFall_Token] <- 1
nodes2$n_genRelatedCases_Later <- 0


i <- 1
for(i in 1:nrow(nodes2)){
   
   curSeqID <- nodes2$SeqID[i]
   indexDate <- nodes2$Meldedatum2[i]
   
   ids <- nodes2$relatedSeqs[i]
   ids <- strsplit(ids, ",")[[1]]
   
   curNodes <- nodes2[nodes2$SeqID %in% ids,]
   curNodes$timeDiff <- curNodes$Meldedatum2 - indexDate
   curNodes <- curNodes[curNodes$timeDiff > 0,]
   
   if(nrow(curNodes) > 0){
   
      cc <- expand.grid(curSeqID, curNodes$SeqID[!curNodes$SeqID %in% curSeqID])
      
      cc$Var1 <- as.character(cc$Var1)
      cc$Var2 <- as.character(cc$Var2)
      
      cc$dist <- as.numeric(lapply(1:nrow(cc), function(x){
         
         getGeneticDistance(cc$Var1[x], cc$Var2[x])
         
      }))
      
      ## Number of community cases with low genetic distance
      acc <- sum(cc$dist < 2)
      lowestDist <- min(cc$dist)
      
      nodes2$n_genRelatedCases_Later[i] <- acc
   
   }
}

# cases mit 0 vs >0 Contact Tracing Records
# für beide Kategorien ein Histogramm über die Zahl der 
# a) genetisch gelinkten Cases insgesamt und 
# b) genetisch gelinkte Cases mit Registrierungsdatum >
#    Registrierungsdatum Index Case erstellen?

library(ggplot2)
library(dplyr)
library(patchwork)

# filter and recode values >= 10
df <- nodes2 %>%
   filter(contacts %in% c(0, 1)) %>%
   mutate(
      n_genRelatedCases_Later_binned = ifelse(n_genRelatedCases_Later >= 10, "≥10", as.character(n_genRelatedCases_Later)),
      n_genRelatedCases_binned       = ifelse(n_genRelatedCases >= 10, "≥10", as.character(n_genRelatedCases))
   )


df$n_genRelatedCases_Later_binned <- factor(df$n_genRelatedCases_Later_binned, levels=c(0:9, "≥10"))
df$n_genRelatedCases_binned <- factor(df$n_genRelatedCases_binned, levels=c(0:9, "≥10"))


# Later cases
p1 <- ggplot(df, aes(x = n_genRelatedCases_Later_binned, fill = factor(contacts))) +
   geom_bar(position = "dodge") +
   labs(x = "Number of Related Cases (Later)",
        y = "Frequency",
        fill = "Contacts") +
   theme_minimal()

# Original cases
p2 <- ggplot(df, aes(x = n_genRelatedCases_binned, fill = factor(contacts))) +
   geom_bar(position = "dodge") +
   labs(x = "Number of Related Cases",
        y = "Frequency",
        fill = "Contacts") +
   theme_minimal()

# combine plots
gg <- p2 + p1

#ggsave(gg, filename="number of genetically related cases per category of contact tracing available.png",
       width = 10, height = 5)


library(dplyr)

# Later cases
df_prop1 <- df %>%
   count(contacts, n_genRelatedCases_Later_binned) %>%
   group_by(contacts) %>%
   mutate(prop = n / sum(n))

p1 <- ggplot(df_prop1, aes(x = n_genRelatedCases_Later_binned,
                           y = prop, fill = factor(contacts))) +
   geom_col(position = "dodge") +
   labs(x = "Number of Related Cases (Later)",
        y = "Proportion within contacts group",
        fill = "Contacts") +
   theme_minimal()

# Original cases
df_prop2 <- df %>%
   count(contacts, n_genRelatedCases_binned) %>%
   group_by(contacts) %>%
   mutate(prop = n / sum(n))

p2 <- ggplot(df_prop2, aes(x = n_genRelatedCases_binned,
                           y = prop, fill = factor(contacts))) +
   geom_col(position = "dodge") +
   labs(x = "Number of Related Cases",
        y = "Proportion within contacts group",
        fill = "Contacts") +
   theme_minimal()

gg <- p2 + p1

#ggsave(gg, filename="Rel Amount of genetically related cases per category of contact tracing available.png",
       width = 10, height = 5)



### make it a relative amount

df <- data.frame(contacts=c(0,1), freq=0, type="n_cases")
df2 <- data.frame(contacts=c(0,1), freq=0, type="n_casn_sequencedCaseses")
df3 <- data.frame(contacts=c(0,1), freq=0, type="genRelCases")
df4 <- data.frame(contacts=c(0,1), freq=0, type="genRelCasesLater")

df$freq[df$contacts == 0] <- sum(nodes3$contacts==0)
df$freq[df$contacts == 1] <- sum(nodes3$contacts==1)

df2$freq[df2$contacts == 0] <- sum(nodes2$contacts==0)
df2$freq[df2$contacts == 1] <- sum(nodes2$contacts==1)

df3$freq[df3$contacts == 0] <- sum(nodes2$n_genRelatedCases[nodes2$contacts == 0]>0)
df3$freq[df3$contacts == 1] <- sum(nodes2$n_genRelatedCases[nodes2$contacts == 1]>0)

df4$freq[df4$contacts == 0] <- sum(nodes2$n_genRelatedCases_Later[nodes2$contacts == 0]>0)
df4$freq[df4$contacts == 1] <- sum(nodes2$n_genRelatedCases_Later[nodes2$contacts == 1]>0)


dfAll <- rbind(df, df2, df3, df4)

dfAll$percentage <- 0

dfAll$percentage[dfAll$contacts == 0 & dfAll$type == "n_casn_sequencedCaseses" ] <-  dfAll$freq[dfAll$contacts == 0 & dfAll$type == "n_casn_sequencedCaseses" ] / dfAll$freq[dfAll$contacts == 0 & dfAll$type == "n_cases"] 
dfAll$percentage[dfAll$contacts == 1 & dfAll$type == "n_casn_sequencedCaseses" ] <-  dfAll$freq[dfAll$contacts == 1 & dfAll$type == "n_casn_sequencedCaseses" ] / dfAll$freq[dfAll$contacts == 1 & dfAll$type == "n_cases"] 

dfAll$percentage[dfAll$contacts == 0 & dfAll$type == "genRelCases" ] <-  dfAll$freq[dfAll$contacts == 0 & dfAll$type == "genRelCases" ] / dfAll$freq[dfAll$contacts == 0 & dfAll$type == "n_casn_sequencedCaseses"] 
dfAll$percentage[dfAll$contacts == 1 & dfAll$type == "genRelCases" ] <-  dfAll$freq[dfAll$contacts == 1 & dfAll$type == "genRelCases" ] / dfAll$freq[dfAll$contacts == 1 & dfAll$type == "n_casn_sequencedCaseses"] 

dfAll$percentage[dfAll$contacts == 0 & dfAll$type == "genRelCasesLater" ] <-  dfAll$freq[dfAll$contacts == 0 & dfAll$type == "genRelCasesLater" ] / dfAll$freq[dfAll$contacts == 0 & dfAll$type == "n_casn_sequencedCaseses"] 
dfAll$percentage[dfAll$contacts == 1 & dfAll$type == "genRelCasesLater" ] <-  dfAll$freq[dfAll$contacts == 1 & dfAll$type == "genRelCasesLater" ] / dfAll$freq[dfAll$contacts == 1 & dfAll$type == "n_casn_sequencedCaseses"] 


library(ggplot2)
library(dplyr)

# labels only where percentage > 0
df_labels <- dfAll %>% filter(percentage > 0)

ggplot(dfAll, aes(x = factor(contacts), y = freq, fill = type)) +
   geom_bar(stat = "identity", position = "identity") +  # overlapping bars
   geom_text(data = df_labels %>% filter(type != "n_cases"),
             aes(label = scales::percent(percentage, accuracy = 0.1),
                 y = freq + max(dfAll$freq) * 0.01),  # small offset above bar
             #position = position_dodge(width = 0.9),
             vjust = 0)+
   labs(x = "Contacts", y = "Frequency", fill = "Case Type") +
   theme_minimal()


## Make list for classification
nodes2$contactRecords <- 0

nodes2$contactRecords[nodes2$CaseID %in% KPpos_classification$IndexFall_Token] <- 1

test <- nodes2[nodes2$contactRecords == 0,]

seqNoRec <- data.frame(CaseID=test$CaseID, n_genRelatedCases= test$n_genRelatedCases)

seqNoRec_random <- seqNoRec[sample(c(1:nrow(seqNoRec), nrow(seqNoRec))),]
seqNoRec_random <- unique(seqNoRec_random)
seqNoRec_random$type <- "A"

seqNoRec_noGenLink <- seqNoRec[seqNoRec$n_genRelatedCases == 0,]
seqNoRec_noGenLink_random <- seqNoRec_noGenLink[sample(c(1:nrow(seqNoRec_noGenLink), nrow(seqNoRec_noGenLink))),]
seqNoRec_noGenLink_random <- unique(seqNoRec_noGenLink_random)
seqNoRec_noGenLink_random$type <- "B"

seqNoRec_GenLink <- seqNoRec[seqNoRec$n_genRelatedCases >= 10,]
seqNoRec_GenLink_random <- seqNoRec_GenLink[sample(c(1:nrow(seqNoRec_GenLink), nrow(seqNoRec_GenLink))),]
seqNoRec_GenLink_random <- unique(seqNoRec_GenLink_random)
seqNoRec_GenLink_random$type <- "C"

##Make preliminary list
# Example: df1, df2, df3 all have same columns
chunk_bind <- function(df1, df2, df3, n1 = 6, n2 = 2, n3 = 2) {
   out <- list()
   i <- 1
   j <- 1
   k <- 1
   repeat {
      part1 <- if (i <= nrow(df1)) df1[i:min(i+n1-1, nrow(df1)), ] else NULL
      part2 <- if (j <= nrow(df2)) df2[j:min(j+n2-1, nrow(df2)), ] else NULL
      part3 <- if (k <= nrow(df3)) df3[k:min(k+n3-1, nrow(df3)), ] else NULL
      
      if (is.null(part1) & is.null(part2) & is.null(part3)) break
      
      out <- append(out, list(part1, part2, part3))
      
      i <- i + n1
      j <- j + n2
      k <- k + n3
   }
   do.call(rbind, out)
}

# Usage
df4 <- chunk_bind(seqNoRec_random, seqNoRec_noGenLink_random, seqNoRec_GenLink_random, n1 = 6, n2 = 2, n3 = 2)

df4$row_ID <- 1:nrow(df4)

GA_list <- df4

GA_list$caseID_duplicated <- duplicated(GA_list$CaseID)

GA_list <- GA_list[GA_list$caseID_duplicated == F,]
GA_list$caseID_duplicated <- NULL

# write.csv2(GA_list, file="GA_CaseID list.csv", row.names=F)
# write.csv2(df4, file="GA_CaseID list unfiltered.csv", row.names=F)


## Plot mit index case number per context label und dazu Nummer an genetisch gelinkten Cases, die Zahl an Kontaktpersonen
## Vorgängen die den jeweiligen Kontext zugeordnet sind, vs. alle Kontaktpersonen-Vorgängen, die zu einem der
## Index cases vebundnen sind (bzw. als Index Token reported wurden)

## create context CaseID lists
contextList <- data.frame(context=unique(KPpos_classification$context), associatedCases="",
                          n_cases=0, n_sequencedCases=0, n_genLinkedCases =0,
                          n_genLinkedCases_unique=0, n_contactCases=0, n_contactCasesContext=0)
load("nodes2_singletons")

i <- 1
for(i in 1:nrow(contextList)){
   
   ctx <- contextList$context[i]
   subSet <- KPpos_classification[KPpos_classification$context == ctx,]
   idxs <- as.character(subSet$IndexFall_Token)
   idxsKP <- as.character(subSet$contect_CaseID)
   idxsKP <- idxsKP[idxsKP != ""]
   
   ids <- c(idxs, idxsKP)
   ids <- unique(ids)
   
   contextList$associatedCases[i] <- paste0(ids, collapse = ",")
   
   ## Get number of cases
   contextList$n_cases[i] <- length(ids)
   ids_sequenced <- ids[ids %in% nodes2$CaseID]
   
   ## Get number of sequenced cases
   contextList$n_sequencedCases[i] <- length(ids_sequenced)
   
   ## Get number of genetically linked cases to sequenced cases
   ids_sequenced_genLinked <- nodes2$relatedSeqs[nodes2$CaseID %in% ids_sequenced]
   ids_sequenced_genLinked <- ids_sequenced_genLinked[ids_sequenced_genLinked != ""]
   ids_sequenced_genLinked <- paste(ids_sequenced_genLinked, collapse = ",")
   ids_sequenced_genLinked <- strsplit(ids_sequenced_genLinked, ",")[[1]]
   ids_sequenced_genLinked <- unique(ids_sequenced_genLinked)
   
   contextList$n_genLinkedCases_unique[i] <- length(ids_sequenced_genLinked)
   contextList$n_genLinkedCases[i] <- sum(nodes2$n_genRelatedCases[nodes2$CaseID %in% ids_sequenced])
   
   ## Get number of contact cases
   curContacts <- edge2[edge2$CaseID_Index %in% ids_sequenced & edge2$Typ == "SurvnetKontaktperson",]
   curContacts <- unique(curContacts$CaseID_Kontaktperson)
   
   contextList$n_contactCases[i] <- length(curContacts)
   rm(curContacts)
   
   curContacts <- edge2[edge2$CaseID_Index %in% ids_sequenced & edge2$Typ == "SurvnetKontaktperson",]
   curContacts <- curContacts[curContacts$context == ctx,]
   curContacts <- unique(curContacts$CaseID_Kontaktperson)
   
   contextList$n_contactCasesContext[i] <- length(curContacts)
   
}


rate <- contextList
rate$Context <- rate$context

rate$Context[rate$Context =="Private Houshold"] <- "Private\nHoushold"
rate$Context[rate$Context =="Recreational context"] <- "Recreational\nContext"
rate$Context[rate$Context =="Kindergarten/Daycare"] <- "Kindergarten/ \nDaycare"
rate$Context[rate$Context =="Residential Care Facility"] <- "Residential\nCare Facility"

rate$Context[rate$Context =="Medical Care facility"] <- "Medical Care\nfacility"
rate$Context[rate$Context =="Community Accommodation"] <- "Community\nAccommodation"
rate$Context[rate$Context =="Refugee Accommodation"] <- "Refugee\nAccommodation"
 
library(ggplot2)
library(ggbreak)
library(scales)
library(dplyr)
library(purrr)


rate$n_genLinkedCases_unique
rate$n_contactCases
rate$n_contactCasesContext

library(dplyr)
library(tidyr)
library(ggplot2)

 
rate_mod <- rate

# Step 3: define context order based on genetic links
context_order <- rate_mod %>%
   arrange(desc(n_genLinkedCases_unique)) %>%
   pull(Context)

# Step 4: reshape into long format
df_long <- rate_mod %>%
   pivot_longer(
      cols = c(n_genLinkedCases_unique, n_contactCases, n_contactCasesContext),
      names_to = "CaseType",
      values_to = "Count"
   ) %>%
   mutate(
      Percentage = 100 * Count / n_sequencedCases,
      Context = factor(Context, levels = c(context_order[-which(context_order %in% c("Others", "NA"))],c("Others", "NA"))),  # enforce order
      CaseType = factor(CaseType,
                        levels = c("n_genLinkedCases_unique",
                                   "n_contactCases",
                                   "n_contactCasesContext"))
   )

# Step 5: make grouped bar plot
df_long$Context

g2 <- ggplot(df_long, aes(x = Context, y = Count, fill = CaseType)) +
   geom_col(position = position_dodge(width = 0.9)) +
   labs(y = "Count", x = "Context", fill = "Case Type") +
   scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB"))

#ggsave(g2, filename="Unique linked Cases per contact case context.png", width = 20, height = 5)

## Make separate plot with rates 


library(dplyr)
library(ggplot2)

# Calculate rate and 95% CI (Wilson)
df <- df_long

library(dplyr)
library(ggplot2)

df_ci <- df %>%
   rowwise() %>%
   mutate(
      rate = Count / n_sequencedCases,
      ci = list(poisson.test(Count, T = n_sequencedCases)$conf.int) # T = exposure
   ) %>%
   mutate(
      ci_lower = ci[[1]],
      ci_upper = ci[[2]]
   ) %>%
   ungroup()

library(paletteer)
palette <- paletteer_d("ggsci::springfield_simpsons") 




library(dplyr)
library(ggplot2)
library(paletteer)

# Assuming your data is in df_long
df <- df_long

df <- df[df$CaseType != "n_contactCasesContext",]

# Calculate rate and 95% CI (Wilson)
df_ci <- df %>%
   rowwise() %>%
   mutate(
      rate = Count / n_sequencedCases,
      ci = list(poisson.test(Count, T = n_sequencedCases)$conf.int)
   ) %>%
   mutate(
      ci_lower = ci[[1]],
      ci_upper = ci[[2]]
   ) %>%
   ungroup()

df_ci$CaseType <- as.character(df_ci$CaseType )
df_ci$CaseType[df_ci$CaseType == "n_genLinkedCases_unique"] <- "Genetically linked"
df_ci$CaseType[df_ci$CaseType == "n_contactCases"] <- "Forward contact tracing-linked"

df_ci$CaseType <- factor(df_ci$CaseType, levels=c("Genetically linked", "Forward contact tracing-linked"))

df_ci <- df_ci[df_ci$n_sequencedCases >= 10,]

# Define palette
palette <- paletteer_d("ggsci::springfield_simpsons") 
palette <- palette[c(1,5,10,11,12,2,6,7,9,4,8,13,3)]
palette <- c("#66C2A5", "#FC8D62", "#8DA0CB", palette)
palette <- palette[-7]

# Combined plot (grouped bars)
ggplot(df_ci, aes(x = Context, y = rate, fill = CaseType)) +
   geom_col(position = position_dodge(width = 0.8)) +
   geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper, group = CaseType),
      position = position_dodge(width = 0.8),
      width = 0.2
   ) +
   labs(
      title = "",
      y = "Linked cases per sequenced context case",
      x = "Context"
   ) +
   scale_fill_manual(values = palette) +
   scale_y_continuous(breaks = seq(0, ceiling(max(df_ci$ci_upper)), by = 1)) +
   theme_bw() +
   theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)
   )

ggsave( filename="Unique linked Cases per contact case context minimal 10 seq cases RATE.png", width = 9, height = 5)

df_ci2 <- data.frame(df_ci)
df_ci2$ci <- NULL
df_ci2$associatedCases <-  NULL
write.csv2(df_ci2, file="test.csv", row.names=F)



ggplot(test, aes(x = reorder(category, sequenced_cases), 
               y = sequenced_cases)) +
   geom_col() +
   coord_flip() +
   labs(
      x = "Category",
      y = "Sequenced cases",
      title = "Sequenced Cases by Category"
   ) 

#ggsave(filename="Sequenced cases per Context category.png", width = 6, height = 4)
