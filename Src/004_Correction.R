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

# get corrections for original dataset
glob <-PAMcorrection::TuningCorr_df(df)
type <-PAMcorrection::TuningCorr_df(df,att1 = "type")
gene <-PAMcorrection::TuningCorr_df(df,att1 = "generation")
dial <-PAMcorrection::TuningCorr_df(df,att1 = "dialect")
dicl <-PAMcorrection::TuningCorr_df(df,att1 = "diaClass_hanna")

PAMcorrection::plotPAMcorr(df)
PAMcorrection::plotPAMcorr(type)
PAMcorrection::plotPAMcorr(gene)
PAMcorrection::plotPAMcorr(dial)
PAMcorrection::plotPAMcorr(dicl)

# correction by two attributes
typeGENE <-PAMcorrection::TuningCorr_df(df,att1 = "type", att2 = "generation")
typeDICL <-PAMcorrection::TuningCorr_df(df,att1 = "type", att2 = "diaClass_hanna")
geneDIAL <-PAMcorrection::TuningCorr_df(df,att1 = "generation", att2 = "dialect")
geneDICL <-PAMcorrection::TuningCorr_df(df,att1 = "generation", att2 = "diaClass_hanna")
typeDIAL <-PAMcorrection::TuningCorr_df(df,att1 = "type", att2 = "dialect")

PAMcorrection::plotPAMcorr(typeGENE)
PAMcorrection::plotPAMcorr(geneDICL)
PAMcorrection::plotPAMcorr(typeDICL)
PAMcorrection::plotPAMcorr(geneDIAL)
PAMcorrection::plotPAMcorr(typeDIAL)

# tuning best result
tunewhis <-PAMcorrection::TuningCorr_df(df,att1 = "type", att2 = "dialect",tuning = "whisker")
tunequan <-PAMcorrection::TuningCorr_df(df,att1 = "type", att2 = "dialect",tuning = "quantil")
tune01 <-PAMcorrection::TuningCorr_df(df,att1 = "type", att2 = "dialect",tuning = "treshold",treshold=1.2)
tune02 <-PAMcorrection::TuningCorr_df(df,att1 = "type", att2 = "dialect",tuning = "treshold",treshold=1.3)
tune03 <-PAMcorrection::TuningCorr_df(df,att1 = "type", att2 = "dialect",tuning = "treshold",treshold=1.4)
tune04 <-PAMcorrection::TuningCorr_df(df,att1 = "type", att2 = "dialect",tuning = "treshold",treshold=1.5)
PAMcorrection::plotPAMcorr(typeDIAL,titel ="TYPE&Dialect untuned" )# untuned
PAMcorrection::plotPAMcorr(tunewhis,titel ="Whisker")
PAMcorrection::plotPAMcorr(tunequan,titel ="Quantil")
PAMcorrection::plotPAMcorr(tune01,titel ="Treshold 04")
PAMcorrection::plotPAMcorr(tune02,titel ="Treshold 04")
PAMcorrection::plotPAMcorr(tune03,titel ="Treshold 05")
PAMcorrection::plotPAMcorr(tune04,titel ="Treshold 05")

# alternative dataset (without unequal WSD)
dfa <- df[!(df$type=="WSD"),]

# original
PAMcorrection::plotPAMcorr(dfa ,titel = "without WSD" )# untuned
glob2 <-PAMcorrection::TuningCorr_df(dfa)
type2 <-PAMcorrection::TuningCorr_df(dfa,att1 = "type")
gene2 <-PAMcorrection::TuningCorr_df(dfa,att1 = "generation")
dial2 <-PAMcorrection::TuningCorr_df(dfa,att1 = "dialect")
dicl2 <-PAMcorrection::TuningCorr_df(dfa,att1 = "diaClass_hanna")

PAMcorrection::plotPAMcorr(dfa)
PAMcorrection::plotPAMcorr(glob)
PAMcorrection::plotPAMcorr(type2)
PAMcorrection::plotPAMcorr(gene2)
PAMcorrection::plotPAMcorr(dial2)
PAMcorrection::plotPAMcorr(dicl2)

# correction by two attributes
typeGENE2 <-PAMcorrection::TuningCorr_df(dfa,att1 = "type", att2 = "generation")
typeDICL2 <-PAMcorrection::TuningCorr_df(dfa,att1 = "type", att2 = "diaClass_hanna")
geneDIAL2 <-PAMcorrection::TuningCorr_df(dfa,att1 = "generation", att2 = "dialect")
geneDICL2 <-PAMcorrection::TuningCorr_df(dfa,att1 = "generation", att2 = "diaClass_hanna")
typeDIAL2 <-PAMcorrection::TuningCorr_df(dfa,att1 = "type", att2 = "dialect")

PAMcorrection::plotPAMcorr(typeGENE2)
PAMcorrection::plotPAMcorr(geneDICL2)
PAMcorrection::plotPAMcorr(typeDICL2)
PAMcorrection::plotPAMcorr(geneDIAL2)
PAMcorrection::plotPAMcorr(typeDIAL2)

# Conclusion

# For the original data set the error can be reduced to 0.15
# when for the data without WSD even reduced to 0.11

# Discussion

# using to much classes keads to very low n used for the mean


