### PAM correction

# set envi
getwd()
wds <- "C:/Envimaster/Evaluate_PAM/Src/"
wdd <- "C:/Envimaster/Evaluate_PAM/Data/"

# load package
require(openxlsx)
require(PAMcorrection)

df <-read.csv(file.path(wdd,"full_clean.csv"),row.names = 1)
head(df)
#full <- read.xlsx(file.path(wdd,"full_clean.xlsx"))
#head(full)

# get an overview
PAMcorrection::plotPAMcorr(df)

# use best tuning model
typeDIAL <-PAMcorrection::TuningCorr_df(df,att1 = "type", att2 = "dialect")

# add absolute difference
typeDIAL$div <- abs(typeDIAL$PAM_corr-typeDIAL$CTR)

# plot
PAMcorrection::plotPAMcorr(typeDIAL)

# get histogram of extremes
hist(typeDIAL$div)
which(typeDIAL$div>0.2)

# get df with extremes
df2 <- typeDIAL[which(typeDIAL$div>0.2),]
head(df2)

# write
write.xlsx(df2,file.path(wdd,"ausreißer.xlsx"),overwrite = T)
write.csv(df2,file.path(wdd,"ausreißer.csv"))

# crop extremes from org data
dft07 <- subset(df,!(df$ID %in% typeDIAL$ID[which(typeDIAL$div>0.7) ]))
dft06 <- subset(df,!(df$ID %in% typeDIAL$ID[which(typeDIAL$div>0.6) ]))
dft05 <- subset(df,!(df$ID %in% typeDIAL$ID[which(typeDIAL$div>0.5) ]))
dft04 <- subset(df,!(df$ID %in% typeDIAL$ID[which(typeDIAL$div>0.4) ]))
dft03 <- subset(df,!(df$ID %in% typeDIAL$ID[which(typeDIAL$div>0.3) ]))
dft02 <- subset(df,!(df$ID %in% typeDIAL$ID[which(typeDIAL$div>0.2) ]))

# calculate model for new data
typeDIAL07 <-PAMcorrection::TuningCorr_df(dft07,att1 = "type", att2 = "dialect")
typeDIAL06 <-PAMcorrection::TuningCorr_df(dft06,att1 = "type", att2 = "dialect")
typeDIAL05 <-PAMcorrection::TuningCorr_df(dft05,att1 = "type", att2 = "dialect")
typeDIAL04 <-PAMcorrection::TuningCorr_df(dft04,att1 = "type", att2 = "dialect")
typeDIAL03 <-PAMcorrection::TuningCorr_df(dft03,att1 = "type", att2 = "dialect")
typeDIAL02 <-PAMcorrection::TuningCorr_df(dft02,att1 = "type", att2 = "dialect")

# plot
PAMcorrection::plotPAMcorr(typeDIAL,titel = "org")
PAMcorrection::plotPAMcorr(typeDIAL07,titel = "Threshold 07")
PAMcorrection::plotPAMcorr(typeDIAL06,titel = "Threshold 06")
PAMcorrection::plotPAMcorr(typeDIAL05,titel = "Threshold 05")
PAMcorrection::plotPAMcorr(typeDIAL04,titel = "Threshold 04")
PAMcorrection::plotPAMcorr(typeDIAL03,titel = "Threshold 03")
PAMcorrection::plotPAMcorr(typeDIAL02,titel = "Threshold 02")


# get amount of entries to deleted by threshold
length(which(typeDIAL$div>0.7)) #6
length(which(typeDIAL$div>0.6)) #8
length(which(typeDIAL$div>0.5)) #11
length(which(typeDIAL$div>0.4)) #20
length(which(typeDIAL$div>0.3)) #43
length(which(typeDIAL$div>0.2)) #112

# get max error
max(abs(typeDIAL$PAM_corr-typeDIAL$CTR))
max(abs(typeDIAL07$PAM_corr-typeDIAL07$CTR))
max(abs(typeDIAL06$PAM_corr-typeDIAL06$CTR))
max(abs(typeDIAL05$PAM_corr-typeDIAL05$CTR))
max(abs(typeDIAL04$PAM_corr-typeDIAL04$CTR))
max(abs(typeDIAL03$PAM_corr-typeDIAL03$CTR))
max(abs(typeDIAL02$PAM_corr-typeDIAL02$CTR))

# get % improvemnet for original
(74.94-70.05) /74.94
(74.94-69.98) /74.94
(74.94-67.37) /74.94
(74.94-62.39) /74.94
(74.94-52.93) /74.94
(74.94-36.10) /74.94

