### Get an Idea of the method

# set envi
getwd()
wds <- "C:/Envimaster/Evaluate_PAM/Src/"
wdd <- "C:/Envimaster/Evaluate_PAM/Data/"

# load package
# devtools::install_github("SchoenbergA/PAMcorrection")

# load package
require(PAMcorrection)


# get dummy df
# all tree "dialects" get different CTR values and 1 extrema (to prevent exact mean)
set.seed(112019)
dialect <-rep("norddeutsch",10)
df1 <-as.data.frame(dialect)
df1$PAM <- sample(seq(0.9,2,0.001),10)
df1$CTR <- df1$PAM*0.9           # variable correction for dilect
df1$CTR[1] <-df1$CTR[1]*1.1      # set 1 extrema

dialect <-rep("mitteldeutsch",10)
df2 <-as.data.frame(dialect)
df2$PAM <- sample(seq(0.9,2,0.001),10)
df2$CTR <- df2$PAM*0.8          # variable correction for dilect
df2$CTR[2] <-df2$CTR[2]*1.2     # set 1 extrema

dialect <-rep("suddeutsch",10)
df3 <-as.data.frame(dialect)
df3$PAM <- sample(seq(0.9,2,0.001),10)
df3$CTR <- df3$PAM*0.6         # variable correction for dilect
df3$CTR[3] <-df3$CTR[3]*1.3    # set 1 extrema

df <- rbind(df1,df2,df3)

# cor
cor(df$PAM,df$CTR)

# plot
PAMcorrection::plotPAMcorr(df,al = c(10,20))
#PAMcorrection::plotPAMcorr(df,sortby = "PAM")

# correct
glob <-PAMcorrection::TuningCorr_df(df)
bydia <-PAMcorrection::TuningCorr_df(df,att1 = "dialect")
bydia <-PAMcorrection::TuningCorr_df(df,att1 = "dialect")
tuned <-PAMcorrection::TuningCorr_df(df,att1 = "dialect", tuning = "threshold",threshold = 0.7)
# plot results
PAMcorrection::plotPAMcorr(df)
PAMcorrection::plotPAMcorr(glob)# generally better
PAMcorrection::plotPAMcorr(glob,abs_dif = F,yl = c(-1,3.5))
PAMcorrection::plotPAMcorr(glob)
# equal to LN model :)
lines(predict(lm(df$CTR~df$PAM)),col='black')

PAMcorrection::plotPAMcorr(bydia)# nearly perfect
PAMcorrection::plotPAMcorr(tuned)# does not improve performance


