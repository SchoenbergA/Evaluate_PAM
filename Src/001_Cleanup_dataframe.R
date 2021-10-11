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
table(dfc$PAM) # has "-"
table(dfc$CTR) # has "-" and "n.d."
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

# add ID and reorder Colums
dfc$ID <- 1:nrow(dfc)
dfc <- dfc[, c(12, 1:11)]

head(dfc)
write.xlsx(dfc,file.path(wdd,"full_clean.xlsx"))
write.csv(dfc,file.path(wdd,"full_clean.csv"))
###############################################################################
