#summary Plots Allele Freq.

summary= read.csv(file = "./summary_afreq60_notskew.csv")

summary2= read.csv(file = "./summary_afreq75_notskew.csv")

summary3= read.csv(file = "./summary_afreq85_notskew.csv")

summary4= read.csv(file = "./summary_afreq90_notskew.csv")

summary5= read.csv(file = "./summary_afreq98_notskew.csv")

summary= read.csv(file = "./summary_afreq99_notskew.csv")

###############
#Allele Freq 60
###############
png(file="../images/batch2/pow_allelefreq99m0.png", width=600, height=550)

#no mean effect
plot(summary$sdeffect[summary$meffect==0],summary$pow_lreg[summary$meffect==0], 
     xlab= "Effect on sd", ylab= "Power", type="l", ylim=c(0,1), col="red", lwd=2,
     main="Power: Afreq=60|meffect=0")

lines(summary$sdeffect[summary$meffect==0], summary$pow_rq.t50[summary$meffect==0], 
      xlab= "sdeffect", ylab= "tau.50 power", type="l", col= "purple",lwd=2)

lines(summary$sdeffect[summary$meffect==0], summary$pow_rq.t75[summary$meffect==0], 
      xlab= "sdeffect", ylab= "tau.75 power",type="l", col="#1f70ab",lwd=2)

lines(summary$sdeffect[summary$meffect==0], summary$pow_rq.t95[summary$meffect==0], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#40b828",lwd=2)

lines(summary$sdeffect[summary$meffect==0], summary$pow_quail[summary$meffect==0], 
      xlab= "sdeffect", ylab= "QUAIL power", col= "#d68915",lwd=2)

lines(x=seq(0,3,0.1), y=rep(0.05,length(seq(0,3,0.1))), lwd=1.5, lty=2)
legend( 2, 0.8, legend=c("Mean", "Tau .50", "Tau .75", "Tau .95", "QUAIL"),
        fill=c("red", "purple", "#1f70ab", "#40b828", "#d68915"), title="Regrssion Meodel", cex=0.8)
dev.off()


png(file="../images/batch2/pow_allelefreq99m2.png", width=600, height=550)
#mean effect = 0.2
plot(summary$sdeffect[summary$meffect==0.2],summary$pow_lreg[summary$meffect==0.2], 
     xlab= "Effect on sd", ylab= "Power", type="l", ylim=c(0,1), col="red", lwd=2,
     main="Power: Afreq= 60|meffect=0.2")

lines(summary$sdeffect[summary$meffect==0.2], summary$pow_rq.t50[summary$meffect==0.2], 
      xlab= "sdeffect", ylab= "tau.50 power", type="l", col= "purple",lwd=2)

lines(summary$sdeffect[summary$meffect==0.2], summary$pow_rq.t75[summary$meffect==0.2], 
      xlab= "sdeffect", ylab= "tau.75 power",type="l", col="#1f70ab",lwd=2)

lines(summary$sdeffect[summary$meffect==0.2], summary$pow_rq.t95[summary$meffect==0.2], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#40b828",lwd=2)

lines(summary$sdeffect[summary$meffect==0.2], summary$pow_quail[summary$meffect==0.2], 
      xlab= "sdeffect", ylab= "QUAIL power", col= "#d68915",lwd=2)

lines(x=seq(0,3,0.1), y=rep(0.05,length(seq(0,3,0.1))), lwd=1.5, lty=2)
legend( 2, 0.8, legend=c("Mean", "Tau .50", "Tau .75", "Tau .95", "QUAIL"),
        fill=c("red", "purple", "#1f70ab", "#40b828", "#d68915"), title="Regrssion Meodel", cex=0.8)
dev.off()

##############
######
#Allele Freq 60
#no mean effect
plot(summary$sdeffect[summary$meffect==0],summary$pow_lreg[summary$meffect==0], 
     xlab= "Effect on sd", ylab= "Power", type="l", ylim=c(0,1), col="red", lwd=2,
     main="Power: Afreq=60|meffect=0")

lines(summary$sdeffect[summary$meffect==0], summary$pow_rq.t50[summary$meffect==0], 
      xlab= "sdeffect", ylab= "tau.50 power", type="l", col= "purple",lwd=2)

lines(summary$sdeffect[summary$meffect==0], summary$pow_rq.t75[summary$meffect==0], 
      xlab= "sdeffect", ylab= "tau.75 power",type="l", col="#1f70ab",lwd=2)

lines(summary$sdeffect[summary$meffect==0], summary$pow_rq.t95[summary$meffect==0], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#40b828",lwd=2)

lines(summary$sdeffect[summary$meffect==0], summary$pow_quail[summary$meffect==0], 
      xlab= "sdeffect", ylab= "QUAIL power", col= "#d68915",lwd=2)

lines(x=seq(0,3,0.1), y=rep(0.05,length(seq(0,3,0.1))), lwd=1.5, lty=2)
legend( 2, 0.8, legend=c("Mean", "Tau .50", "Tau .75", "Tau .95", "QUAIL"),
        fill=c("red", "purple", "#1f70ab", "#40b828", "#d68915"), title="Regrssion Meodel", cex=0.8)


#mean effect = 0.2
plot(summary$sdeffect[summary$meffect==0.2],summary$pow_lreg[summary$meffect==0.2], 
     xlab= "Effect on sd", ylab= "Power", type="l", ylim=c(0,1), col="red", lwd=2,
     main="Power: Afreq= 60|meffect=0.2")

lines(summary$sdeffect[summary$meffect==0.2], summary$pow_rq.t50[summary$meffect==0.2], 
      xlab= "sdeffect", ylab= "tau.50 power", type="l", col= "purple",lwd=2)

lines(summary$sdeffect[summary$meffect==0.2], summary$pow_rq.t75[summary$meffect==0.2], 
      xlab= "sdeffect", ylab= "tau.75 power",type="l", col="#1f70ab",lwd=2)

lines(summary$sdeffect[summary$meffect==0.2], summary$pow_rq.t95[summary$meffect==0.2], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#40b828",lwd=2)

lines(summary$sdeffect[summary$meffect==0.2], summary$pow_quail[summary$meffect==0.2], 
      xlab= "sdeffect", ylab= "QUAIL power", col= "#d68915",lwd=2)

lines(x=seq(0,3,0.1), y=rep(0.05,length(seq(0,3,0.1))), lwd=1.5, lty=2)
legend( 2, 0.8, legend=c("Mean", "Tau .50", "Tau .75", "Tau .95", "QUAIL"),
        fill=c("red", "purple", "#1f70ab", "#40b828", "#d68915"), title="Regrssion Meodel", cex=0.8)