### Clean up dataframe

# NOTE: for the "full dataframe"
# The original data has been use and copied 5 times with added CTR and PAM values
# by hand in Exceö

# set envi
getwd()
wds <- "C:/Envimaster/Evaluate_PAM/Src/"
wdd <- "C:/Envimaster/Evaluate_PAM/Data/"

# load package
require(openxlsx)

# load data
full <- openxlsx::read.xlsx(xlsxFile =file.path(wdd,"mod/full.xlsx"))
head(full)

# clean data
# clean rows by "progress lvl"
dfc <-full[!(full$progess_lvl== 0 | full$progess_lvl== 1| full$progess_lvl== 9),]
head(dfc)
tail(dfc)
# check content
table(dfc$PAM) # has "-" 170
table(dfc$CTR) # has "-" and "n.d." 9
# remove
dfc <- dfc[!(dfc$PAM== "-" |dfc$CTR== "-" |dfc$CTR== "n.d." ),]
head(dfc)
# handle NA

which(is.na(dfc$CTR)==T)
dfc[231,]

dfc<-dfc[-231,]
# char to num
dfc$PAM <-as.numeric(dfc$PAM)
dfc$CTR <-as.numeric(dfc$CTR)

# add ID
dfc$ID <- 1:nrow(dfc)
head(dfc)

# get new dialect calssification (by hanna based on Alfred)
dfc$diaClass_hanna <- ifelse(grepl(
  "^Nordniederdeutsch$|^Nordniederdeutsch-Ostfälisch$|^Mecklenburgisch-Vorpommersch$|^Mittelpommersch$|^Brandenburgisch-Südmärkisch$|^Brandenburgisch$"
  , dfc$dilect), "nördliches Niederdeutsch",
  ifelse(grepl(
    "^Westfälisch$|^Ostfälisch$|^Ostfälisch-Brandenburgisch$"
    , dfc$dilect), "südliches Niederdeutsch",
    ifelse(grepl(
      "^Ripuarisch$|^Ripuarisch-Niederfränkisch$|^Moselfränkisch$|^Moselfränkisch-Ripuarisch$|^Niederfränkisch$"
      , dfc$dilect), "Westdeutsch",
      ifelse(grepl(
        "^Rheinfränkisch$|^Rheinfränkisch-Moselfränkisch$|^Rheinfränkisch-Moselfränkisch-Zentralhessisch$|^Rheinfränkisch-Ostfränkisch-Schwäbisch$|^Rheinfränkisch-Zentralhessisch-Moselfränkisch$|^Nordhessisch$|^Zentralhessisch$|^Osthessisch$", dfc$dilect), "westliches Mitteldeutsch",
        ifelse(grepl(
          "^Thüringisch$|^Thüringisch-Obersächsisch$|^Obersächsisch$|^Nordobersächsisch$"
          , dfc$dilect), "östliches Mitteldeutsch",
          ifelse(grepl(
            "^Ostfränkisch$|^Nordbairisch$|^Nordbairisch-Mittelbairisch$|^Mittelbairisch$|^Südbairisch-Schwäbisch$"
            , dfc$dilect), "Ostoberdeutsch",
            ifelse(grepl(
              "^Schwäbisch-Mittelbairisch$|^Schwäbisch$|^Hochalemannisch$|^Hochalemannisch/Niederalemannisch$|^Mittelalemannisch$|^Niederalemannisch$"
              , dfc$dilect), "Westoberdeutsch",
              0)))))))



# check if there is any "0", NOTE: pattern syntax need to have all expressions in a row without " " or enter
unique(dfc$diaClass_hanna)
head(dfc)

# rename columns
colnames(dfc)
colnames(dfc)[colnames(dfc) == 'dilect'] <- 'dialect' # spell correction :D

# reorder columns
dfc <- dfc[, c(12, 1,13,2:11),]
head(dfc)

# get some idea of the distribution of combination

# main classes
table(dfc$dialect)
table(dfc$diaClass_hanna)
table(dfc$generation)
table(dfc$type)

# type by age
table(dfc$type[dfc$generation=="jung"])
table(dfc$type[dfc$generation=="mittel"])
table(dfc$type[dfc$generation=="alt"])

# dialect by age
table(dfc$dialect[dfc$generation=="jung"])
table(dfc$dialect[dfc$generation=="mittel"])
table(dfc$dialect[dfc$generation=="alt"])

write.xlsx(dfc,file.path(wdd,"full_clean.xlsx"),overwrite = T)
write.csv(dfc,file.path(wdd,"full_clean.csv"))# automatic overwrites
###############################################################################
