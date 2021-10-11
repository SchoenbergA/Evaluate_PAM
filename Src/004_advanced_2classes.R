### Advanced 2 class correction

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

# take a look on org data

# calculate overall global mean
gmf <- mean(full$CTR/full$PAM) # factor </>
gma <- mean(full$PAM-full$CTR) # absolute >-<
PAMcorrection::estPAMcorr2(full,yl = 4)

# correct the data by class 'generation' (3 classes)
which(colnames(full)=="generation")
c1_age <-PAMcorrection::Corr_df(full,5)

# check result (factor)
PAMcorrection::estPAMcorr2(full,yl = 4) # org
PAMcorrection::estPAMcorr2(full,yl = 4,cf=gmf) # global corrected
PAMcorrection::estPAMcorr2(c1_age,yl = 4) # crrected by mean for classes age

# correct the data by class 'dilect'
which(colnames(full)=="dilect")
c1_dia <-PAMcorrection::Corr_df(full,2)

# check result (factor)
PAMcorrection::estPAMcorr2(full,yl = 4) # org
PAMcorrection::estPAMcorr2(full,yl = 4,cf=gmf) # global corrected
PAMcorrection::estPAMcorr2(c1_age,yl = 4) # corrected by mean for classes age
PAMcorrection::estPAMcorr2(c1_dia,yl = 4) # corrected by mean for classes dilect
# much better than age

# last test with type
# correct the data by class 'type'
which(colnames(full)=="type")
c1_ty <-PAMcorrection::Corr_df(full,7)

# check result (factor)
PAMcorrection::estPAMcorr2(full,yl = 4) # org
PAMcorrection::estPAMcorr2(full,yl = 4,cf=gmf) # global corrected
PAMcorrection::estPAMcorr2(c1_age,yl = 4) # corrected by mean for classes age
PAMcorrection::estPAMcorr2(c1_dia,yl = 4) # corrected by mean for classes dilect
PAMcorrection::estPAMcorr2(c1_ty,yl = 4) # corrected by mean for classes type
# better than age but worser than dilect

# get 2 class combinations
# correct the data by two classes (factor)
which(colnames(full)=="type")
which(colnames(full)=="generation")
which(colnames(full)=="dilect")
c2_ty_di <-PAMcorrection::Corr_df(full,2,7)
c2_ty_ge <-PAMcorrection::Corr_df(full,5,7)
c2_di_ge <-PAMcorrection::Corr_df(full,2,5)

# best result with one class:'dialect' @149 total Mean 0.24 SD 0.18

PAMcorrection::estPAMcorr2(c2_ty_di,yl = 4)
PAMcorrection::estPAMcorr2(c2_di_ge,yl = 4)
PAMcorrection::estPAMcorr2(c2_ty_ge,yl = 4)

# best result using two classes: type and dilect @ 74 , 0.12, 0.12

# check vs absolutes
ca2_ty_di <-PAMcorrection::Corr_df(full,2,7,modef = F)
ca2_ty_ge <-PAMcorrection::Corr_df(full,5,7,modef = F)
ca2_di_ge <-PAMcorrection::Corr_df(full,2,5,modef = F)

PAMcorrection::estPAMcorr2(ca2_ty_di,yl = 4) # factor still better
PAMcorrection::estPAMcorr2(ca2_di_ge,yl = 4)
PAMcorrection::estPAMcorr2(ca2_ty_ge,yl = 4)

