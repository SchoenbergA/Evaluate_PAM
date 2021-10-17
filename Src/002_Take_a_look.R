### Take a look on the Data

# set envi
getwd()
wds <- "C:/Envimaster/Evaluate_PAM/Src/"
wdd <- "C:/Envimaster/Evaluate_PAM/Data/"

# load package
# devtools::install_github("SchoenbergA/PAMcorrection")

# load package
require(openxlsx)
require(PAMcorrection)

full <-read.csv(file.path(wdd,"full_clean.csv"),row.names = 1)
head(full)
full <-na.omit(full$CTR_add)
full
#full <- read.xlsx(file.path(wdd,"full_clean.xlsx"))
#head(full)

full<-full[order(full$CTR_add),]
which(full$CTR_add=="?")  #1:10
which(full$CTR_add=="CK") #11:136
which(full$CTR_add=="LV") #137:138
which(full$CTR_add=="MG") #139:172
which(full$CTR_add=="SL") #173:204
which(is.na(full$CTR_add))#205:624

abl_ctradd <-c(1,11,137,139,173,205)

# get diffenrece (factor)
full$dif <- full$CTR/full$PAM
# boxplot
boxplot(dif~type,data = full)
boxplot(dif~CTR_add,data = full)

# shapirotest normalverteilung
shapiro.test(full$PAM)

# Mann-Whitney-U-Test/Wilcoxon-Test
wilcox.test(full$CTR,full$PAM)

# kruscal - generell
kruskal.test(full$dif~full$generation)
# genauer
pairwise.wilcox.test(full$dif,full$generation, p.adjust="bonferroni")
pairwise.wilcox.test(full$dif,full$type, p.adjust="bonferroni")
pairwise.wilcox.test(full$dif,full$diaClass_hanna, p.adjust="bonferroni")
# take a look
PAMcorrection::plotPAMcorr(full,yl = 4)
PAMcorrection::plotPAMcorr(full,yl = 4,al = abl_ctradd,sortby = "CTR_add")
abline(v=abl_ctradd)
wilcox.test(full$CTR,full$PAM,full$ID)
z <- qnorm(2.2e-16)
r <- z/sqrt(624)
t.test(full$CTR~full$PAM, )
       t.test(x~y, var.equal, alternative))
t.test(full$CTR)
c <-c(1,2,3,2,1)
shapiro.test(c)
shapiro.test(full$CTR)
shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(runif(100, min = 2, max = 4))
qqnorm(full$CTR)
qqline(full$CTR)
qqnorm(runif(100, min = 2, max = 4))
qqline(runif(100, min = 2, max = 4))
# set lines for type
# prepare ablines
unique(full$type)
which(full$type=="WSS")    #   1:130
which(full$type=="NoSo")   # 131:270
which(full$type=="Inter")  # 271:387
which(full$type=="FG")     # 388:506
which(full$type=="WSD")    # 507:624

abl1 <- c(131,271,388,507)
PAMcorrection::plotPAMcorr(full,al = abl1,yl = 4)
glob <-PAMcorrection::TuningCorr_df(df = full)
PAMcorrection::plotPAMcorr(glob[1:40,],al = abl1)
abline(h=0.13)
cor(full$PAM,full$CTR)
cor(full$PAM[1:130],full$CTR[1:130])
cor(full$PAM[131:270],full$CTR[131:270])
cor(full$PAM[271:387],full$CTR[271:387])
cor(full$PAM[388:506],full$CTR[388:506])
cor(full$PAM[507:624],full$CTR[507:624])


PAMcorrection::plotPAMcorr(full[507:624,])
glob <-PAMcorrection::TuningCorr_df(df = full[507:624,])
PAMcorrection::plotPAMcorr(glob)
full[507:527,]
dfc <-   dfc[!(dfc$PAM  == "-"),]
test <- full[!(full$type=="WSD"),]
PAMcorrection::plotPAMcorr(full)
PAMcorrection::plotPAMcorr(test)
glob <-PAMcorrection::TuningCorr_df(df = test)
type <-PAMcorrection::TuningCorr_df(df = test,att1 = "type")
typegen <-PAMcorrection::TuningCorr_df(df = test,att1 = "type",att2 = "generation")
dia <-PAMcorrection::TuningCorr_df(df = test,att1 = "dialect")
diagen <-PAMcorrection::TuningCorr_df(df = test,att1 = "dialect",att2 = "generation")
diagen2 <-PAMcorrection::TuningCorr_df(df = full,att1 = "dialect",att2 = "generation")
PAMcorrection::plotPAMcorr(glob)
PAMcorrection::plotPAMcorr(type)# weil siehe test kein unteshcied zwischen den andere
PAMcorrection::plotPAMcorr(typegen)
PAMcorrection::plotPAMcorr(diagen,sortby = "type")
abline(h=0.11)
PAMcorrection::plotPAMcorr(diagen2,sortby = "type")
PAMcorrection::plotPAMcorr(diagen2[diagen2$type=="WSD",],sortby = "type")
# take a look
PAMcorrection::estPAMcorr2(full,yl = 4,al = abl1)

# calculate overall global mean
gmf <- mean(full$CTR/full$PAM) # factor </>
gma <- mean(full$PAM-full$CTR) # absolute >-<

# check cor
cor(full$CTR,full$PAM)

# check correction
PAMcorrection::estPAMcorr2(full,yl = 2,al = abl1,cf=gmf)
PAMcorrection::estPAMcorr2(full,yl = 4,al = abl1,modef = F,cf=gma)
# absolute values lead to better results but may be less variable for future use.

# take a close look to a single type (using global correction)
PAMcorrection::estPAMcorr2(full[1:130,],al = abl1)
PAMcorrection::estPAMcorr2(full[1:130,],al = abl1,cf=gmf)
PAMcorrection::estPAMcorr2(full[1:130,],al = abl1)
PAMcorrection::estPAMcorr2(full[1:130,],al = abl1,modef = F,cf=gma)
