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

# add column for difference
full$diff <- abs(full$PAM-full$CTR)

# simple cortest
cor(full$PAM,full$CTR) # high cor 0.907412

# stats test for generation

# plot
boxplot(full$PAM~full$generation)
boxplot(full$CTR~full$generation)
boxplot(full$diff~full$generation)

# anova
summary(aov(full$diff~full$generation))# significant due to ***

# TukeyHSD
TukeyHSD(aov(full$diff~full$generation))# diff (in mean value)


# stats test for type

# plot
boxplot(full$PAM~full$type)
boxplot(full$CTR~full$type)
boxplot(full$diff~full$type)

# anova
summary(aov(full$diff~full$type))# significant

# TukeyHSD
TukeyHSD(aov(full$diff~full$type))# diff (in mean value)

# stats test for dialect

# plot
boxplot(full$PAM~full$dilect)
boxplot(full$CTR~full$dilect)
boxplot(full$diff~full$dilect)


boxplot(full$diff[1:130]~full$dilect[1:130])
boxplot(full$diff[131:270]~full$dilect[131:270])

# anova
summary(aov(full$diff~full$dilect))# significant
# TukeyHSD
TukeyHSD(aov(full$diff~full$dilect))# diff (in mean value)

