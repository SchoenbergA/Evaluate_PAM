### Take a look on the Data

# set envi
getwd()
wds <- "C:/Envimaster/Evaluate_PAM/Src/"
wdd <- "C:/Envimaster/Evaluate_PAM/Data/"

# load package
require(openxlsx)
require(PAMcorrection)

full <-read.csv(file.path(wdd,"full_clean.csv"),row.names = 1)
head(full)
#full <- read.xlsx(file.path(wdd,"full_clean.xlsx"))
#head(full)

# take a look
PAMcorrection::estPAMcorr2(full,yl = 4)

# set lines for type
# prepare ablines
unique(full$type)
which(full$type=="WSS")    #   1:130
which(full$type=="NoSo")   # 131:270
which(full$type=="Inter")  # 271:387
which(full$type=="FG")     # 388:506
which(full$type=="WSD")    # 507:624

abl1 <- c(131,271,388,507)

# take a look
PAMcorrection::estPAMcorr2(full,yl = 4,al = abl1)

# calculate overall global mean
gmf <- mean(full$CTR/full$PAM) # factor </>
gma <- mean(full$PAM-full$CTR) # absolute >-<

# check cor
cor(full$CTR,full$PAM)

# check correction
PAMcorrection::estPAMcorr2(full,yl = 4,al = abl1,cf=gmf)
PAMcorrection::estPAMcorr2(full,yl = 4,al = abl1,modef = F,cf=gma)
# absolute values lead to better results but may be less variable for future use.

# take a close look to a single type (using global correction)
PAMcorrection::estPAMcorr2(full[1:130,],al = abl1)
PAMcorrection::estPAMcorr2(full[1:130,],al = abl1,cf=gmf)
PAMcorrection::estPAMcorr2(full[1:130,],al = abl1)
PAMcorrection::estPAMcorr2(full[1:130,],al = abl1,modef = F,cf=gma)
