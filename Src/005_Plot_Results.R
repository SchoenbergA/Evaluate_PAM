### PAM correction

# set envi
getwd()
wds <- "C:/Envimaster/Evaluate_PAM/Src/"
wdd <- "C:/Envimaster/Evaluate_PAM/Data/"
require(reshape2)
require(tidyverse)
require(ggplot2)
require(dplyr)
require(gridExtra)

# load package
require(openxlsx)
require(PAMcorrection)

df <-read.csv(file.path(wdd,"full_clean.csv"),row.names = 1)
head(df)
#full <- read.xlsx(file.path(wdd,"full_clean.xlsx"))
#head(full)

typeDIAL <-PAMcorrection::TuningCorr_df(df,att1 = "type", att2 = "dialect")

# prepare data for ggplot
dfm <- select(typeDIAL, informant, type, PAM, CTR, PAM_corr)
dfm <-melt(dfm,id.vars = c("informant", "type"))
names(dfm)
head(dfm)

# delete entries
# get names of informant with less than 15 entries
table(dfm$informant)
wh <-which(table(dfm$informant)!=15)
nwh <-names(wh) # get list of names
dfn <- dfm # get new df
for (i in 1:length(nwh)) {

  dfn <- dfn[!dfn$informant==nwh[i],]
  print(i)
  print(nwh[i])
}

# get df without PAM_corr (for visualisation)
dft <- dfn[!dfn$variable=="PAM_corr",]

# plot only informants with all types (PAM and CTR only)
ggplot(dft, aes(type, value, col=type, shape=variable)) +
  geom_point() +
  facet_wrap(~informant) +
  scale_shape_manual(values=c(2,1,4))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

# plot only informants with all types
ggplot(dfn, aes(type, value, col=type, shape=variable)) +
  geom_point() +
  facet_wrap(~informant) +
  scale_shape_manual(values=c(2,1,4))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())



# plot single informants
p1 <-ggplot(dft[dft$informant=="G2",], aes(type, value, col=type, shape=variable))+
  geom_point(cex = 6,stroke=2)+
  facet_wrap(~informant) +
  scale_shape_manual(values=c(2,1,4))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())+ theme(legend.position = "none")

p1


# alternative style
p2 <-ggplot(dfn[dfn$informant=="G2",], aes(type, value, col=type, shape=variable))+
  geom_point(cex = 6,stroke=2)+
  facet_wrap(~informant) +
  scale_shape_manual(values=c(2,1,4))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())+ theme(legend.position = "none")

p2

# plot grid
grid.arrange(p1, p2,p1,p2, nrow = 2)

