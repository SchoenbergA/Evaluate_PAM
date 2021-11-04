
# set envi
getwd()
wds <- "C:/Envimaster/Evaluate_PAM/Src/"
wdd <- "C:/Envimaster/Evaluate_PAM/Data/"
require(reshape2)
require(tidyverse)
require(ggplot2)
require(dplyr)
require(gridExtra)

# -------------------------------------------------------------------------



# data --------------------------------------------------------------------
mess <-read.csv2(file.path(wdd,"d-mess-sel.csv"),stringsAsFactors = F)
head(mess)
mess <- mess[,1:14]
mess <- na.omit(mess)
mess <- mess[,3:ncol(mess)]


# z transformation --------------------------------------------------------
mess.scale <- apply(mess[,3:ncol(mess)], 2, scale) %>%
  as.data.frame()


# first view --------------------------------------------------------------
boxplot(mess[,3:ncol(mess)])
boxplot(mess.scale)


# individual t.tests ------------------------------------------------------
t.test(mess.scale$PAM.Wert_WSS, mess.scale$Kontrollwert_WSS)
t.test(mess.scale$PAM.Wert_NOSO, mess.scale$Kontrollwert_NOSO)
t.test(mess.scale$PAM.Wert_INT, mess.scale$Kontrollwert_INT)
t.test(mess.scale$PAM.Wert_FG, mess.scale$Kontrollwert_FG)
t.test(mess.scale$PAM.Wert_WSD, mess.scale$Kontrollwert_WSD)


# ANOVA via Kruskal-Wallis due to non-normal data -------------------------
pairwise.wilcox.test(c(mess.scale$PAM.Wert_WSS,
                       mess.scale$PAM.Wert_NOSO,
                       mess.scale$PAM.Wert_INT,
                       mess.scale$PAM.Wert_FG,
                       mess.scale$PAM.Wert_WSD),
                     c(mess.scale$Kontrollwert_WSS,
                       mess.scale$Kontrollwert_NOSO,
                       mess.scale$Kontrollwert_INT,
                       mess.scale$Kontrollwert_FG,
                       mess.scale$Kontrollwert_WSD),
                     p.adjust = "bonferroni")



# -------------------------------------------------------------------------
# chi square per subject --------------------------------------------------
messPAM <- select(mess.scale, PAM.Wert_WSS, PAM.Wert_NOSO, PAM.Wert_INT, PAM.Wert_FG, PAM.Wert_WSD)
colnames(messPAM) <- c("WSS", "NOSO", "INT", "FG", "WSD")
messCOR <- select(mess.scale, Kontrollwert_WSS, Kontrollwert_NOSO, Kontrollwert_INT, Kontrollwert_FG, Kontrollwert_WSD)
colnames(messCOR) <- c("WSS", "NOSO", "INT", "FG", "WSD")


ch <- list()
for (i in 1:nrow(messPAM)) {
  ch[i] <- chisq.test(t(messPAM[i,]), t(messCOR[i,]))$p.value
}
ch %>% unlist()

# regression (all contexts integrated) ------------------------------------
vals.pam <- messPAM[, 1:5]
vals <- data.frame(pam = c(t(vals.pam)), stringsAsFactors=F)
vals.cor <- messCOR[, 1:5]
vals$cor <- c(t(vals.cor))

plot(vals$pam, vals$cor)
lm(pam~cor, data = vals)
cor.test(vals$pam, vals$cor)


# preparation for plotting ------------------------------------------------
messPAM$Informant <- mess$Informant
messPAM$Generation <- mess$GENERATION

messPAM.t <- select(messPAM, WSS,NOSO,INT,FG,WSD) %>%
  t() %>% as.data.frame()
colnames(messPAM.t) <- mess$Informant
messPAM.t$cond <- "PAM"
messPAM.t$context <- rownames(messPAM.t)

messCOR$Informant <- mess$Informant
messCOR$Generation <- mess$GENERATION

messCOR.t <- select(messCOR, WSS,NOSO,INT,FG,WSD) %>%
  t() %>% as.data.frame()
colnames(messCOR.t) <- mess$Informant
messCOR.t$cond <- "COR"
messCOR.t$context <- rownames(messCOR.t)

mess.all <- rbind(messPAM.t, messCOR.t)


# data for plotting -------------------------------------------------------
mess.all <- melt(mess.all, id.vars = c("context", "cond"))


# plots -------------------------------------------------------------------
# per condition
ggplot(mess.all, aes(context, value, col = context)) +
  geom_jitter() +
  facet_wrap(~cond)

# per speaker
ggplot(mess.all, aes(context, value, col = context, shape = cond)) +
  geom_point() +
  facet_wrap(~variable) +
  scale_shape_manual(values=c(2,1,4))+
    theme(axis.text = element_blank(),
        axis.ticks = element_blank())

# per generation
mess.all.alt <- mess.all %>%
  filter(grepl("ALT", mess.all$variable))
ggplot(mess.all.alt, aes(context, value, col = context, shape = cond)) +
  scale_shape_manual(values=c(2,1,4))+
    geom_point() +
  facet_wrap(~variable)

mess.all.mittel <- mess.all %>%
  filter(!grepl("(JUNG|ALT)", mess.all$variable))
ggplot(mess.all.mittel, aes(context, value, col = context, shape = cond)) +
    geom_point() +
  scale_shape_manual(values=c(2,1,4))+
  facet_wrap(~variable)

mess.all.jung <- mess.all %>%
  filter(grepl("JUNG", mess.all$variable))
ggplot(mess.all.jung, aes(context, value, col = context, shape = cond)) +
  scale_shape_manual(values=c(2,1,4))+
    geom_point() +
  facet_wrap(~variable)
# -------------------------------------------------------------------------


# factor for value correction ---------------------------------------------
## log way is not suited for application
## even though it seems to be perfect from the perspective of test statistics
PAM <- select(mess, PAM.Wert_WSS, PAM.Wert_NOSO, PAM.Wert_INT, PAM.Wert_FG, PAM.Wert_WSD)
colnames(messPAM) <- c("WSS", "NOSO", "INT", "FG", "WSD")
COR <- select(mess, Kontrollwert_WSS, Kontrollwert_NOSO, Kontrollwert_INT, Kontrollwert_FG, Kontrollwert_WSD)
colnames(messCOR) <- c("WSS", "NOSO", "INT", "FG", "WSD")

# data
vals.pam.raw <- PAM[, 1:5]
vals.raw <- data.frame(pam = c(t(vals.pam.raw)), stringsAsFactors=F)
vals.cor.raw <- COR[, 1:5]
vals.raw$cor <- c(t(vals.cor.raw))

plot(vals.raw$pam, vals.raw$cor)

# factor
corr.factor <- lm(pam~cor, data = vals.raw)$coefficients[1]/diff(range(vals.raw$cor))


# application
# vals.raw$pamIntercept.rel <- vals.raw$pam - logb(vals.raw$pam*corr.factor, .1)
vals.raw$pamIntercept.rel <- vals.raw$pam - vals.raw$pam*corr.factor * 1.1
vals.raw$pamIntercept <- vals.raw$pam - lm(pam~cor, data = vals.raw)$coefficients[1]
vals.raw$lfd <- 1:nrow(vals.raw)

# test
plot(vals.raw$pamIntercept.rel, vals.raw$cor)
boxplot(vals.raw$pamIntercept.rel, vals.raw$cor)

cor.test(vals.raw$pamIntercept.rel, vals.raw$cor)
cor.test(vals.raw$pam, vals.raw$cor)
cor.test(vals.raw$pam, vals.raw$pamIntercept.rel)

t.test(vals.raw$pamIntercept.rel, vals.raw$cor)

vals.ord <- vals.raw[order(vals.raw$cor),]
vals.ord$lfd <- 1:nrow(vals.ord)
ggplot(vals.ord, aes(lfd, cor)) +
  geom_line() +
  geom_point(aes(lfd, pamIntercept.rel), col = "red", size = 1) +
#  geom_point(aes(lfd, pam), col = "blue", size = .5, alpha = .5) +
  geom_errorbar(aes(ymin = cor-sd(cor),
                    ymax = cor+sd(cor)), alpha = .3,
                position=position_dodge(0.05))

grid.arrange(ggplot(vals.ord, aes(lfd, cor)) +
               geom_line() +
               ggtitle("PAM raw") +
               labs(x ="", y = "") +
               geom_point(aes(lfd, pam), col = "blue", size = .5, alpha = .5) +
               geom_errorbar(aes(ymin = cor-sd(cor),
                                 ymax = cor+sd(cor)), alpha = .1,
                             position=position_dodge(0.05)),
             ggplot(vals.ord, aes(lfd, cor)) +
               geom_line() +
               ggtitle("PAM - intercept correction") +
               labs(x ="", y = "") +
               geom_point(aes(lfd, pamIntercept), col = "green", size = .5) +
               geom_errorbar(aes(ymin = cor-sd(cor),
                                 ymax = cor+sd(cor)), alpha = .1,
                             position=position_dodge(0.05)),
             ggplot(vals.ord, aes(lfd, cor)) +
               geom_line() +
               ggtitle("PAM - log.1(interc. corr.)") +
               labs(x ="", y = "") +
               geom_point(aes(lfd, pamIntercept.rel), col = "red", size = .5) +
               geom_errorbar(aes(ymin = cor-sd(cor),
                                 ymax = cor+sd(cor)), alpha = .1,
                             position=position_dodge(0.05)),
             ncol = 3)

# -------------------------------------------------------------------------


# data for plotting -------------------------------------------------------
mess.raw <- mess[,c(3,5,7,9,11)]
mess.raw <- data.frame(value = c(t(mess.raw)),
                       stringsAsFactors=F)
mess.raw$intercept <- mess.raw$value - mess.raw$value * corr.factor * .5
mess.raw$context <- rep(c("WSS", "NOSO", "INT", "FG", "WSD"), 20)
mess.raw$cond <- "PAM"

cor.test(mess.raw$value, mess.raw$intercept)

mess.sel <- mess[,c(4,6,8,10,12)]
mess.sel <- data.frame(value = c(t(mess.sel)), stringsAsFactors=F)
mess.sel$intercept <- mess.sel$value
mess.sel$context <- rep(c("WSS", "NOSO", "INT", "FG", "WSD"), 20)
mess.sel$cond <- "COR"

mess.raw <- rbind(mess.raw, mess.sel)
mess.raw$variable <- mess.all$variable

plot(mess.raw$value, mess.raw$intercept)
cor.test(filter(mess.raw, cond == "PAM")$intercept, filter(mess.raw, cond == "COR")$value)


# per condition
ggplot(mess.raw, aes(context, intercept, col = context)) +
  geom_point() +
  facet_wrap(~cond)

# per speaker
ggplot(mess.raw, aes(context, intercept, col = context, shape = cond)) +
  geom_point() +
  facet_wrap(~variable) +
  labs(x="", y="") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

# test
cor.test(mess.raw$value, mess.raw$intercept)
cor.test(filter(mess.raw, cond == "PAM")$intercept,
         filter(mess.raw, cond == "COR")$intercept)
cor.test(vals$pam, vals$cor)



# result ------------------------------------------------------------------
# Some specialists correct very carefully (cf. Tuttlingen)
# others more intensively (cf. Büdingen).
# That is, the correction leads to better validity but causes
# problems of objectivity.
# As the whole procedure is problematic in terms of objectivity
# corrections - if put together - increase this problem.
#
# intercept correction is suited in order to fit the data distribution in statistical terms,
# but it messes the individual relations => not for usage







