### Some Statistics

# set envi
getwd()
wds <- "C:/Envimaster/Evaluate_PAM/Src/"
wdd <- "C:/Envimaster/Evaluate_PAM/Data/"

# load package
require(openxlsx)

full <-read.csv(file.path(wdd,"full_clean.csv"),row.names = 1)
head(full)
#full <- read.xlsx(file.path(wdd,"full_clean.xlsx"))
#head(full)

# get positions for type
unique(full$type)
which(full$type=="WSS")    #   1:130
which(full$type=="NoSo")   # 131:270
which(full$type=="Inter")  # 271:387
which(full$type=="FG")     # 388:506
which(full$type=="WSD")    # 507:624

# add column for difference
full$diffa <- abs(full$PAM-full$CTR)
full$difff <- abs(full$CTR/full$PAM)

# test for normal distribution (p Alpha >= 0,05 == normal dist)
shapiro.test(full$PAM)
shapiro.test(full$CTR)
shapiro.test(full$PAM)
shapiro.test(full$diffa)
shapiro.test(full$difff)
# all are NOT norm dist

# global correlation
cor.test(full$PAM,full$CTR,method = "pearson")# 0.907412  "global", effect "High" (Cohen 1988)

# cor in type
cor.test(full$PAM[1:130],full$CTR[1:130])     # 0.7732981 "WSS"
cor.test(full$PAM[131:270],full$CTR[131:270]) # 0.7866801 "NoSo"
cor.test(full$PAM[271:387],full$CTR[271:387]) # 0.7206394 "Inter"
cor.test(full$PAM[388:506],full$CTR[388:506]) # 0.8941178 "FG"
cor.test(full$PAM[507:624],full$CTR[507:624]) # 0.9477936 "WSD"

# cor generation
jun <-subset(full,full$generation=="jung")
mit <-subset(full,full$generation=="mittel")
alt <-subset(full,full$generation=="alt")

cor.test(jun$PAM,jun$CTR) # 0.7885659
cor.test(mit$PAM,mit$CTR) # 0.9186516
cor.test(alt$PAM,alt$CTR) # 0.9437992

### Mean comparison

# plot generation
boxplot(full$PAM~full$generation)
boxplot(full$CTR~full$generation)
boxplot(full$difff~full$generation)

# more boxplots
boxplot(full$difff~full$dialect)
boxplot(full$difff~full$diaClass_hanna, xaxt = "n", yaxt = "n")## Draw x-axis without labels.
        axis(side = 1, labels = FALSE)

        ## Draw y-axis.
        axis(side = 2,
             ## Rotate labels perpendicular to y-axis.
             las = 2,
             ## Adjust y-axis label positions.
             mgp = c(3, 0.75, 0))

        ## Draw the x-axis labels.
        text(x = 1:length(unique(full$diaClass_hanna)),
             ## Move labels to just below bottom of chart.
             y = par("usr")[3] ,
             ## Use names from the data list.
             labels = unique(full$diaClass_hanna),
             ## Change the clipping region.
             xpd = NA,
             ## Rotate the labels by 35 degrees.
             srt = 35,
             ## Adjust the labels to almost 100% right-justified.
             adj = 0.965,
             ## Increase label size.
             cex = 0.8)
boxplot(full$difff~full$type)

# Mann-Whitney-U-Test/Wilcoxon-Test

# kruscal - generell
kruskal.test(full$difff~full$type) # is unequal
kruskal.test(full$difff~full$dialect)# is unequal
kruskal.test(full$difff~full$generation)# is unequal

# more exact (>= 0.05 == nearly equal)
pairwise.wilcox.test(full$difff,full$generation, p.adjust="bonferroni") # all unequal
pairwise.wilcox.test(full$difff,full$type, p.adjust="bonferroni") # WSD unequal to all other
pairwise.wilcox.test(full$difff,full$diaClass_hanna, p.adjust="bonferroni")# some equal to each other



