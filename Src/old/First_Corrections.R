# set envi
getwd()
wds <- "C:/Envimaster/Evaluate_PAM/Src/"
wdd <- "C:/Envimaster/Evaluate_PAM/Data/"


# load package
require(openxlsx)
require(mapview)
require(sp)
require(rgdal)
require(raster)
require(PAMcorrection)

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

## add dialect low lvl
#dfc$dil_0 <- ifelse("Nordniederdeutsch|Mecklenburgisch-Vorpommersch|Brandenburgisch", dfc$dilect), "nord",)
#hunde$type <- ifelse(grepl("Nordniederdeutsch|Mecklenburgisch-Vorpommersch|Brandenburgisch", dfc$dilect), "nd",
#                     ifelse(grepl("ng|n.g", hunde$hunde), "ng",
#                            ifelse(grepl("nn|n$", hunde$hunde), "nn", 0)))

# add ID and reorder Colums
dfc$ID <- 1:nrow(dfc)
head(dfc)
dfc <- dfc[, c(12, 1:11)]

vis <- dfc[order(dfc$PAM),]
plot(vis$PAM)
plot(vis$CTR)
###############################################################################

# prepare ablines
unique(dfc$type)
which(dfc$type=="WSS")    #   1:130
which(dfc$type=="NoSo")   # 131:270
which(dfc$type=="Inter")  # 271:387
which(dfc$type=="FG")     # 388:506
which(dfc$type=="WSD")    # 507:624

abl1 <- c(131,271,388,507)

# take a look

# full
PAMcorrection::estPAMcorr(dfc$CTR,dfc$PAM,al = abl1,yl=4)
PAMcorrection::estPAMcorr(vis$CTR,vis$PAM,al = abl1,yl=4)
## by age
#dage <-dfc[order(dfc$type,dfc$PAM),]
#which(dage$generation=="jung") # 2
#which(dage$generation=="mittel") # 3
#which(dage$generation=="alt") # 1##
#
#ablage <- c(226,398)
#
#PAMcorrection::estPAMcorr(dage$CTR,dage$PAM,al = ablage,yl=4)
#PAMcorrection::estPAMcorr(dage$CTR,dage$PAM,al = abl1,yl=4,modef = F,cf = 0.45)


PAMcorrection::estPAMcorr(dfc$CTR[1:130],dfc$PAM[1:130])
PAMcorrection::estPAMcorr(dfc$CTR[131:270],dfc$PAM[131:270],yl=4)
PAMcorrection::estPAMcorr(dfc$CTR[271:387],dfc$PAM[271:387],yl=4)
PAMcorrection::estPAMcorr(dfc$CTR[388:506],dfc$PAM[388:506],yl=4)
PAMcorrection::estPAMcorr(dfc$CTR[507:624],dfc$PAM[507:624],yl=4)

# cor
cor(dfc$CTR,dfc$PAM) # global
cor(dfc$CTR[1:130],dfc$PAM[1:130])
cor(dfc$CTR[131:270],dfc$PAM[131:270])
cor(dfc$CTR[271:387],dfc$PAM[271:387])
cor(dfc$CTR[388:506],dfc$PAM[388:506])
cor(dfc$CTR[507:624],dfc$PAM[507:624])

# Corr factor by mean

# glob div (fact) - control / pam
divf <-dfc$CTR/dfc$PAM
mean(divf) # 0.702

corr_wss <-mean(dfc$CTR[1:130]/dfc$PAM[1:130])     # 0.651
corr_NosO <-mean(dfc$CTR[131:270]/dfc$PAM[131:270]) # 0.673
corr_Inter <-mean(dfc$CTR[271:387]/dfc$PAM[271:387]) # 0.647
corr_FG <-mean(dfc$CTR[388:506]/dfc$PAM[388:506]) # 0.703
corr_WSD <-mean(dfc$CTR[507:624]/dfc$PAM[507:624]) # 0.846

# corrected
PAMcorrection::estPAMcorr(dfc$CTR[1:130],dfc$PAM[1:130])
PAMcorrection::estPAMcorr(dfc$CTR[1:130],dfc$PAM[1:130],cf = corr_wss)
PAMcorrection::estPAMcorr(dfc$CTR[131:270],dfc$PAM[131:270])
PAMcorrection::estPAMcorr(dfc$CTR[131:270],dfc$PAM[131:270],cf = corr_NosO)
PAMcorrection::estPAMcorr(dfc$CTR[271:387],dfc$PAM[271:387],yl=3)
PAMcorrection::estPAMcorr(dfc$CTR[271:387],dfc$PAM[271:387],yl=3,cf = corr_Inter)
PAMcorrection::estPAMcorr(dfc$CTR[388:506],dfc$PAM[388:506],yl=3)
PAMcorrection::estPAMcorr(dfc$CTR[388:506],dfc$PAM[388:506],yl=3,cf = corr_FG)
PAMcorrection::estPAMcorr(dfc$CTR[507:624],dfc$PAM[507:624],yl=4)
PAMcorrection::estPAMcorr(dfc$CTR[507:624],dfc$PAM[507:624],yl=4,cf = corr_WSD)

# corr on full

dfc2 <-dfc
dfc2$PAM[1:130]<-dfc2$PAM[1:130]*corr_wss
dfc2$PAM[131:270]<-dfc2$PAM[131:270]*corr_NosO
dfc2$PAM[271:387]<-dfc2$PAM[271:387]*corr_Inter
dfc2$PAM[388:506]<-dfc2$PAM[388:506]*corr_FG
dfc2$PAM[507:624]<-dfc2$PAM[507:624]*corr_WSD

cor(dfc$CTR,dfc$PAM) # global
cor(dfc2$CTR,dfc2$PAM) # global by type corrected


mean(dfc$CTR/dfc$PAM)
mean(dfc2$CTR/dfc2$PAM)

# global after correction
PAMcorrection::estPAMcorr(dfc$CTR,dfc$PAM,al = abl1,yl=3) # org
PAMcorrection::estPAMcorr(dfc$CTR,dfc$PAM,al = abl1,cf = 0.702,yl=3) # global corrected
PAMcorrection::estPAMcorr(dfc2$CTR,dfc2$PAM,al = abl1,yl=3) # corrected by type

### correction type by age ################################################################

dfc_order <- dfc[1:130,]
#dfc_order <- dfc_order[order(dfc_order$PAM),]
PAMcorrection::estPAMcorr(dfc_order$CTR[1:130],dfc_order$PAM[1:130])
PAMcorrection::estPAMcorr(dfc_order$CTR[1:130],dfc_order$PAM[1:130],cf = corr_wss)

# sep age for example WSS
unique(dfc_order$generation)

wss1 <-subset(dfc_order[1:130,], generation == "jung")
wss2 <-subset(dfc_order[1:130,], generation == "mittel")
wss3 <-subset(dfc_order[1:130,], generation == "alt")

# cortest
cor(dfc_order$CTR[1:130],dfc_order$PAM[1:130]) # WSS
cor(wss1$CTR,wss1$PAM)
cor(wss2$CTR,wss2$PAM)
cor(wss3$CTR,wss3$PAM)

m1 <-mean(wss1$CTR/wss1$PAM)
m2 <-mean(wss2$CTR/wss2$PAM)
m3 <-mean(wss3$CTR/wss3$PAM)

PAMcorrection::estPAMcorr(wss1$CTR,wss1$PAM)
PAMcorrection::estPAMcorr(wss1$CTR,wss1$PAM,cf = m1)
PAMcorrection::estPAMcorr(wss2$CTR,wss2$PAM)
PAMcorrection::estPAMcorr(wss2$CTR,wss2$PAM,cf = m2)
PAMcorrection::estPAMcorr(wss3$CTR,wss3$PAM)
PAMcorrection::estPAMcorr(wss3$CTR,wss3$PAM,cf = m3)

wss <- dfc_order[1:130,]

# add corrected vlaues
wss$PAM[wss$generation=="jung"] <-wss$PAM[wss$generation=="jung"]*m1
wss$PAM[wss$generation=="mittel"] <-wss$PAM[wss$generation=="mittel"]*m2
wss$PAM[wss$generation=="alt"] <-wss$PAM[wss$generation=="alt"]*m3

cor(dfc_order$CTR[1:130],dfc_order$PAM[1:130])
cor(wss$PAM,wss$CTR) # increased cor

PAMcorrection::estPAMcorr(dfc$CTR[1:130],dfc$PAM[1:130])
PAMcorrection::estPAMcorr(dfc_order$CTR[1:130],dfc_order$PAM[1:130])
PAMcorrection::estPAMcorr(dfc_order$CTR[1:130],dfc_order$PAM[1:130],cf = corr_wss)
PAMcorrection::estPAMcorr(wss$CTR,wss$PAM)

df1 <- dfc_order

# one step forward
##########################################################
dfc_order <- dfc[271:387,]
#dfc_order <- dfc_order[order(dfc_order$PAM),]
PAMcorrection::estPAMcorr(dfc_order$CTR,dfc_order$PAM)
PAMcorrection::estPAMcorr(dfc_order$CTR,dfc_order$PAM,cf = corr_wss)

# sep age for example WSS
unique(dfc_order$generation)

wss1 <-subset(dfc_order, generation == "jung")
wss2 <-subset(dfc_order, generation == "mittel")
wss3 <-subset(dfc_order, generation == "alt")

# cortest
cor(dfc_order$CTR,dfc_order$PAM) # INTER
cor(wss1$CTR,wss1$PAM)
cor(wss2$CTR,wss2$PAM)
cor(wss3$CTR,wss3$PAM)

m1 <-mean(wss1$CTR/wss1$PAM)
m2 <-mean(wss2$CTR/wss2$PAM)
m3 <-mean(wss3$CTR/wss3$PAM)

PAMcorrection::estPAMcorr(wss1$CTR,wss1$PAM)
PAMcorrection::estPAMcorr(wss1$CTR,wss1$PAM,cf = m1)
PAMcorrection::estPAMcorr(wss2$CTR,wss2$PAM)
PAMcorrection::estPAMcorr(wss2$CTR,wss2$PAM,cf = m2)
PAMcorrection::estPAMcorr(wss3$CTR,wss3$PAM)
PAMcorrection::estPAMcorr(wss3$CTR,wss3$PAM,cf = m3)

wss <- dfc_order

# add corrected vlaues
wss$PAM[wss$generation=="jung"] <-wss$PAM[wss$generation=="jung"]*m1
wss$PAM[wss$generation=="mittel"] <-wss$PAM[wss$generation=="mittel"]*m2
wss$PAM[wss$generation=="alt"] <-wss$PAM[wss$generation=="alt"]*m3

cor(dfc_order$CTR,dfc_order$PAM)
cor(wss$PAM,wss$CTR) # increased cor

PAMcorrection::estPAMcorr(dfc_order$CTR,dfc_order$PAM)
PAMcorrection::estPAMcorr(dfc_order$CTR,dfc_order$PAM,cf = corr_wss)
PAMcorrection::estPAMcorr(wss$CTR,wss$PAM)

df3 <- dfc_order
####################################
dfc_order <- dfc[131:270,]
#dfc_order <- dfc_order[order(dfc_order$PAM),]
PAMcorrection::estPAMcorr(dfc_order$CTR,dfc_order$PAM)
PAMcorrection::estPAMcorr(dfc_order$CTR,dfc_order$PAM,cf = corr_wss)

# sep age for example WSS
unique(dfc_order$generation)

wss1 <-subset(dfc_order, generation == "jung")
wss2 <-subset(dfc_order, generation == "mittel")
wss3 <-subset(dfc_order, generation == "alt")

# cortest
cor(dfc_order$CTR,dfc_order$PAM) # INTER
cor(wss1$CTR,wss1$PAM)
cor(wss2$CTR,wss2$PAM)
cor(wss3$CTR,wss3$PAM)

m1 <-mean(wss1$CTR/wss1$PAM)
m2 <-mean(wss2$CTR/wss2$PAM)
m3 <-mean(wss3$CTR/wss3$PAM)

PAMcorrection::estPAMcorr(wss1$CTR,wss1$PAM)
PAMcorrection::estPAMcorr(wss1$CTR,wss1$PAM,cf = m1)
PAMcorrection::estPAMcorr(wss2$CTR,wss2$PAM)
PAMcorrection::estPAMcorr(wss2$CTR,wss2$PAM,cf = m2)
PAMcorrection::estPAMcorr(wss3$CTR,wss3$PAM)
PAMcorrection::estPAMcorr(wss3$CTR,wss3$PAM,cf = m3)

wss <- dfc_order

# add corrected vlaues
wss$PAM[wss$generation=="jung"] <-wss$PAM[wss$generation=="jung"]*m1
wss$PAM[wss$generation=="mittel"] <-wss$PAM[wss$generation=="mittel"]*m2
wss$PAM[wss$generation=="alt"] <-wss$PAM[wss$generation=="alt"]*m3

cor(dfc_order$CTR,dfc_order$PAM)
cor(wss$PAM,wss$CTR) # increased cor


PAMcorrection::estPAMcorr(dfc_order$CTR,dfc_order$PAM)
PAMcorrection::estPAMcorr(dfc_order$CTR,dfc_order$PAM,cf = corr_wss)
PAMcorrection::estPAMcorr(wss$CTR,wss$PAM)

df2 <- dfc_order

test <- rbind(df1,df2,df3)
#test <- test[order(test$ID),]

PAMcorrection::estPAMcorr(dfc$CTR[1:387],dfc$PAM[1:387],al = abl1,yl=3) # org
PAMcorrection::estPAMcorr(dfc$CTR[1:387],dfc$PAM[1:387],al = abl1,cf = 0.702,yl=3) # global corrected
PAMcorrection::estPAMcorr(dfc2$CTR[1:387],dfc2$PAM[1:387],al = abl1,yl=3,cf=1.1) # by type
PAMcorrection::estPAMcorr(test$CTR,test$PAM,yl=3)

PAMcorrection::estPAMcorr(df1$CTR,df1$PAM)
PAMcorrection::estPAMcorr(df2$CTR,df2$PAM)
PAMcorrection::estPAMcorr(df3$CTR,df3$PAM)

