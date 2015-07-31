#Reading in the raw datasets and defining functions that convert
#factor to numeric and that find cols with NA values

RawAdv <- read.csv("BR_GeneralAdvancedStats.csv")
RawBas <- read.csv("BR_GeneralBasicStats.csv")
RawBasPer36 <- read.csv("BR_GeneralBasicStatsPer36Min.csv")
RawBasPer100 <- read.csv("BR_GeneralBasicStatsPer100Poss.csv")
RawBasPerGame <- read.csv("BR_GeneralBasicStatsPerGame.csv")
Raw_EEFGP <- read.csv("NC_ExpectedEffectiveFGP.csv")
RawRim <- read.csv("NC_RimProtection.csv")
RawUsg <- read.csv("NC_TrueUsage.csv")

asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, 
                      lapply(d[, sapply(d, is.factor)], asNumeric))

#I only use this function interactively
nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) any(is.na(x))))]
}

#Cleaning Basketball Reference Data

# A. Keep only the first instance of each player name. This is done
#because players who played on multiple teams will have multiple
#observations in the dataset. One of these observations is the total
#over all teams and this is always listed first in the BR datasets.
# B. Remove cols & rows within the datasets that contain no 
#pertinent information and drop the unused factor levels. Unneeded rows
#can be identified by having the column headers as the values. This is
#why I remove all rows where the Player column takes the value "Player."
# C. Convert factors to numeric where necessary.
# D. Remove row names from the processed data.
# E. Rename columns where desired.
# F. Repeat for each set of BR data.
Adv <- RawAdv[match(unique(RawAdv$Player), RawAdv$Player),]
Adv <- subset(Adv, Player != "Player")
Adv <- Adv[,c(-1,-20,-25)]
Adv <- droplevels(Adv)
Adv[,c(3,5:26)] <- factorsNumeric(Adv)[,c(3,5:26)]
row.names(Adv) <- NULL

Bas <- RawBas[match(unique(RawBas$Player), RawBas$Player),]
Bas <- subset(Bas, Player != "Player")
Bas <- Bas[,-1]
Bas <- droplevels(Bas)
Bas[,c(3,5:29)] <- factorsNumeric(Bas)[,c(3,5:29)]
row.names(Bas) <- NULL

BasPer100 <- RawBasPer100[match(unique(RawBasPer100$Player), 
                                RawBasPer100$Player),]
BasPer100 <- subset(BasPer100, Player != "Player")
BasPer100 <- BasPer100[,c(-1,-30)]
BasPer100 <- droplevels(BasPer100)
BasPer100[,c(3,5:30)] <- factorsNumeric(BasPer100)[,c(3,5:30)]
row.names(BasPer100) <- NULL
colnames(BasPer100) <- paste(colnames(BasPer100), "Per100Poss", 
                             sep = "_")
colnames(BasPer100)[1] <- "Player"

BasPer36 <- RawBasPer36[match(unique(RawBasPer36$Player), 
                                RawBasPer36$Player),]
BasPer36 <- subset(BasPer36, Player != "Player")
BasPer36 <- BasPer36[,-1]
BasPer36 <- droplevels(BasPer36)
BasPer36[,c(3,5:28)] <- factorsNumeric(BasPer36)[,c(3,5:28)]
row.names(BasPer36) <- NULL
colnames(BasPer36) <- paste(colnames(BasPer36), "Per36Min", 
                            sep = "_")
colnames(BasPer36)[1] <- "Player"

BasPerGame <- RawBasPerGame[match(unique(RawBasPerGame$Player), 
                              RawBasPerGame$Player),]
BasPerGame <- subset(BasPerGame, Player != "Player")
BasPerGame <- BasPerGame[,-1]
BasPerGame <- droplevels(BasPerGame)
BasPerGame[,c(3,5:29)] <- factorsNumeric(BasPerGame)[,c(3,5:29)]
row.names(BasPerGame) <- NULL
colnames(BasPerGame) <- paste(colnames(BasPerGame), "PerGame", 
                              sep = "_")
colnames(BasPerGame)[1] <- "Player"

#Cleaning Nylon Calculus Data

#Aggregate NC usage data by player. This combines the observations of
#players who played for multiple teams by averaging their statistics
#as this dataset lacks a single combined observation for those players
#who changed teams during the season. Averaging is an imperfect
#solution as contexts change as players change teams, sample sizes
#vary, etc. We can take action if we believe this will corrupt an
#analysis and more precision is required. 
Usg <- aggregate(. ~ Player, RawUsg, mean)

#Coerce Player to character and change names where necessary to match
#BR data.
Rim <- RawRim
Rim$Player <- as.character(Rim$Player)
Rim$Player[49] <- gsub(Rim$Player[49], "JJ Hickson", "J.J. Hickson")
Rim$Player[69] <- gsub(Rim$Player[69], "Nene", "Nene Hilario")
Rim$Player[105] <- gsub(Rim$Player[105], "Lou Amudson", 
                        "Louis Amudson")

#And again
EEFGP <- Raw_EEFGP
EEFGP$Player <- as.character(EEFGP$Player)
EEFGP$Player[46] <- gsub(EEFGP$Player[46], "JJ Redick", "J.J. Redick")
EEFGP$Player[70] <- gsub(EEFGP$Player[70], "CJ Miles", "C.J. Miles")
EEFGP$Player[103] <- gsub(EEFGP$Player[103], "Tim Hardaway Jr.", 
                          "Tim Hardway")
EEFGP$Player[136] <- gsub(EEFGP$Player[136], "Nene", "Nene Hilario")
EEFGP$Player[138] <- gsub(EEFGP$Player[138], "PJ Tucker", 
                          "P.J. Tucker")
EEFGP$Player[153] <- gsub(EEFGP$Player[153], "Jose Juan Barea", 
                          "Jose Barea")
EEFGP$Player[191] <- gsub(EEFGP$Player[191], "JJ Hickson", 
                          "J.J. Hickson")
EEFGP$Player[206] <- gsub(EEFGP$Player[206], "KJ McDaniels", 
                          "K.J. McDaniels")
EEFGP$Player[210] <- gsub(EEFGP$Player[210], "CJ Watson", 
                          "C.J. Watson")

#use plyr's join_all to merge data frames. Make a list of df's
#from most complete to least complete, then join.
library(plyr)
dflist <- list(BasPer100, Bas, BasPerGame, BasPer36, Adv, Usg, 
               EEFGP, Rim)
MasterData <- join_all(dflist, by="Player", type="full")

#Remove rows 493-511 which are full of NAs. The observtions represent
#players that only saw extremely limited action or players where the 
#name was changed in the NC datasets and rows with the old name
#remain in the dataset.
MasterData <- MasterData[1:492,]
#Next, remove uneeded columns
MasterData <- MasterData[,c(1,8:9,11:12,14:15,17:18,20:58,65:66,68:69,71:72,
                       75:76,78:86,93:94,96:97,99:100,102:103,105:133,
                       136:145,147:151)]

#Rename variables. Tried to this all at once, but kept getting an error.
colnames(MasterData)[19] <- "ORtg"
colnames(MasterData)[20] <- "DRtg"
colnames(MasterData)[29] <- "FGPerct"
colnames(MasterData)[30] <- "3P"
colnames(MasterData)[31] <- "3PA"
colnames(MasterData)[32] <- "3PPerct"
colnames(MasterData)[33] <- "2P"
colnames(MasterData)[34] <- "2PA"
colnames(MasterData)[35] <- "2PPerct"
colnames(MasterData)[36] <- "Eff_FGPerct"
colnames(MasterData)[39] <- "FTPerct"
colnames(MasterData)[51] <- "3P_PerGame"
colnames(MasterData)[52] <- "3PA_PerGame"
colnames(MasterData)[53] <- "2P_PerGame"
colnames(MasterData)[54] <- "2PA_PerGame"
colnames(MasterData)[68] <- "3P_Per36Min"
colnames(MasterData)[69] <- "3PA_Per36Min"
colnames(MasterData)[70] <- "2P_Per36Min"
colnames(MasterData)[71] <- "2PA_Per36Min"
colnames(MasterData)[84] <- "TrueShootingPerct"
colnames(MasterData)[85] <- "3PA_Rate"
colnames(MasterData)[86] <- "FTA_Rate"
colnames(MasterData)[87] <- "ORBPerct"
colnames(MasterData)[88] <- "DRBPerct"
colnames(MasterData)[89] <- "TRBPerct"
colnames(MasterData)[90] <- "ASTPerct"
colnames(MasterData)[91] <- "STLPerct"
colnames(MasterData)[92] <- "BLKPerct"
colnames(MasterData)[93] <- "TOVPerct"
colnames(MasterData)[94] <- "USGPerct"
colnames(MasterData)[98] <- "WS_Per48Min"
colnames(MasterData)[107] <- "TrueTOVPerct"
colnames(MasterData)[108] <- "TimeOfPossPerct"
colnames(MasterData)[109] <- "Shot/Touch_Perct"
colnames(MasterData)[110] <- "PerctOfTouchesEndingPoss"
colnames(MasterData)[112] <- "Expected_Eff_FGPerct"
colnames(MasterData)[113] <- "Def_Eff_FGPerct"
colnames(MasterData)[114] <- "ContestPerct"
colnames(MasterData)[115] <- "Rim_FGPerct_Allowed"
colnames(MasterData)[116] <- "PointsSaved_PerGame"
colnames(MasterData)[117] <- "PointsSaved_Per36Min"

#reorder columns
MasterData <- MasterData[,c(1,21:48,83:102,19:20,49:65,2:18,66:82,
                            103:117)]
