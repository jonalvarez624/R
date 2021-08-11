install.packages("fitdistrplus")
library("fitdistrplus")
data.stuff <- data.frame("DTF"= c(runif(2000, 1, 1100)))
#Replace zeroes to 1 due to error in fitdist function
data.stuff$DTF[data.stuff$DTF < 1] <- 1
#Plot raw data in EDF and CDF
plotdist(data.stuff$DTF, histo = TRUE, demp = TRUE)
#Output summary statistics Cullen and Frey graph
#descdist(data.stuff$DTF, boot = 1000)

#fit all feasible continuous distribution models
fw <- fitdist(data.stuff$DTF, "weibull")
fgm <- fitdist(data.stuff$DTF, "gamma")
fln <- fitdist(data.stuff$DTF, "lnorm")
fn <- fitdist(data.stuff$DTF, "norm")
fl <- fitdist(data.stuff$DTF, "logis")
fc <- fitdist(data.stuff$DTF, "cauchy")

MLEstat <- gofstat(list(fw,fgm,fln,fn,fl,fc))

#Building a data frame with all test statistics for each distribution
Eval <-data.frame(
                 "AIC" = c(fw$aic,fgm$aic,fln$aic,fn$aic,fl$aic,fc$aic), 
                 "BIC" = c(fw$bic,fgm$bic,fln$bic,fn$bic,fl$bic,fc$bic),
                 "MLE KS Statistic" = c(MLEstat$ks),
                 "MLE CVM Statistic" = c(MLEstat$cvm),
                 "MLE AD Statistic" = c(MLEstat$ad))
Eval$AICRank = rank(Eval$AIC)
Eval$BICRank = rank(Eval$BIC)
Eval$KSRank = rank(Eval$MLE.KS.Statistic)
Eval$CVMRank = rank(Eval$MLE.CVM.Statistic)
Eval$ADRank = rank(Eval$MLE.AD.Statistic)
Eval$FinalRank = rowMeans(Eval[,c("AICRank","BICRank","KSRank","CVMRank","ADRank")])                      
                      
#Model summary statistics
summary(fw)
summary(fgm)
summary(fln)
summary(fn)
summary(fl)
summary(fc)

#Visual plot of all distributions EDF,CDF,QQ,PP
plot.legend <- c("Weibull", "gamma","lognormal","normal","logistic","cauchy")
denscomp(list(fw,fgm,fln,fn,fl,fc), legendtext = plot.legend)
cdfcomp(list(fw,fgm,fln,fn,fl,fc), legendtext = plot.legend)
#qqcomp(list(fw,fgm,fln,fn,fl,fc), legendtext = plot.legend, xlim = c(.01,max(data.stuff$DTF)))
#ppcomp(list(fw,fgm,fln,fn,fl,fc), legendtext = plot.legend)