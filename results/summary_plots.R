summary= read.csv(file = "./results/summary_table.csv")
summary= read.csv(file = "./results/summary_n1nots_v2.csv")


summary2= read.csv(file = "./results/summary_n1000s.csv")
summary2= read.csv(file = "./results/summary_n1000s_v2.csv")

simulation = read.csv(file= "./simulations_n5000nots.csv")
summary3= read.csv(file = "./summary_n5000nots.csv")

##########
#Not Skewd
##########
#no mean effect
plot(summary$sdeffect[summary$meffect==0],summary$pow_lreg[summary$meffect==0], 
     xlab= "Effect on sd", ylab= "Power", type="l", ylim=c(0,1), col="red", lwd=2,
     main="Power on regression models: Type B")

lines(summary$sdeffect[summary$meffect==0], summary$pow_rq.t50[summary$meffect==0], 
     xlab= "sdeffect", ylab= "tau.50 power", type="l", col= "#4E0C8F",lwd=2)

lines(summary$sdeffect[summary$meffect==0], summary$pow_rq.t75[summary$meffect==0], 
     xlab= "sdeffect", ylab= "tau.75 power",type="l", col="#105C9B",lwd=2)

lines(summary$sdeffect[summary$meffect==0], summary$pow_rq.t90[summary$meffect==0], 
     xlab= "sdeffect", ylab= "tau.90 power", col="#62A7DF",lwd=2)

lines(summary$sdeffect[summary$meffect==0], summary$pow_rq.t95[summary$meffect==0], 
     xlab= "sdeffect", ylab= "tau.95 power", col= "#268F0C",lwd=2)

lines(summary$sdeffect[summary$meffect==0], summary$pow_rq.t95[summary$meffect==0], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#268F0C",lwd=2)

lines(x=seq(0,0.8,0.1), y=rep(0.05,9), lwd=1.5, lty=2)

#0.04 mean effect
plot(summary$sdeffect[summary$meffect==0.04],summary$pow_lreg[summary$meffect==0.04], 
     xlab= "Effect on sd", ylab= "Power", type="l", ylim=c(0,1), col="red", lwd=2,
     main="Power on regression models: meffect = 0.04, sdeffect= x")

lines(summary$sdeffect[summary$meffect==0.04], summary$pow_rq.t50[summary$meffect==0.04], 
      xlab= "sdeffect", ylab= "tau.50 power", type="l", col= "#4E0C8F",lwd=2)

lines(summary$sdeffect[summary$meffect==0.04], summary$pow_rq.t75[summary$meffect==0.04], 
      xlab= "sdeffect", ylab= "tau.75 power",type="l", col="#105C9B",lwd=2)

lines(summary$sdeffect[summary$meffect==0.04], summary$pow_rq.t90[summary$meffect==0.04], 
      xlab= "sdeffect", ylab= "tau.90 power", col="#62A7DF",lwd=2)

lines(summary$sdeffect[summary$meffect==0.04], summary$pow_rq.t95[summary$meffect==0.04], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#268F0C",lwd=2)

lines(summary$sdeffect[summary$meffect==0.04], summary$pow_rq.t95[summary$meffect==0.0], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#268F0C",lwd=2)

lines(x=seq(0,1,0.1), y=rep(0.05,11), lwd=1.5, lty=2)


#0.02 mean effect
plot(summary$sdeffect[summary$meffect==0.02],summary$pow_lreg[summary$meffect==0.02], 
     xlab= "Effect on sd", ylab= "Power", type="l", ylim=c(0,1), col="red", lwd=2,
     main="Power on regression models: Type D")

lines(summary$sdeffect[summary$meffect==0.02], summary$pow_rq.t50[summary$meffect==0.02], 
      xlab= "sdeffect", ylab= "tau.50 power", type="l", col= "#4E0C8F",lwd=2)

lines(summary$sdeffect[summary$meffect==0.02], summary$pow_rq.t75[summary$meffect==0.02], 
      xlab= "sdeffect", ylab= "tau.75 power",type="l", col="#105C9B",lwd=2)

lines(summary$sdeffect[summary$meffect==0.02], summary$pow_rq.t90[summary$meffect==0.02], 
      xlab= "sdeffect", ylab= "tau.90 power", col="#62A7DF",lwd=2)

lines(summary$sdeffect[summary$meffect==0.02], summary$pow_rq.t95[summary$meffect==0.02], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#268F0C",lwd=2)

lines(summary$sdeffect[summary$meffect==0.02], summary$pow_rq.t95[summary$meffect==0.02], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#268F0C",lwd=2)

lines(x=seq(0,1,0.1), y=rep(0.05,11), lwd=1.5, lty=2)


#0.06 mean effect
plot(summary$sdeffect[summary$meffect==0.06],summary$pow_lreg[summary$meffect==0.06], 
     xlab= "Effect on sd", ylab= "Power", type="l", ylim=c(0,1), col="red", lwd=2,
     main="Power on regression models: meffect = 0.06, sdeffect= x")

lines(summary$sdeffect[summary$meffect==0.06], summary$pow_rq.t50[summary$meffect==0.06], 
      xlab= "sdeffect", ylab= "tau.50 power", type="l", col= "#4E0C8F",lwd=2)

lines(summary$sdeffect[summary$meffect==0.06], summary$pow_rq.t75[summary$meffect==0.06], 
      xlab= "sdeffect", ylab= "tau.75 power",type="l", col="#105C9B",lwd=2)

lines(summary$sdeffect[summary$meffect==0.06], summary$pow_rq.t90[summary$meffect==0.06], 
      xlab= "sdeffect", ylab= "tau.90 power", col="#62A7DF",lwd=2)

lines(summary$sdeffect[summary$meffect==0.06], summary$pow_rq.t95[summary$meffect==0.06], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#268F0C",lwd=2)

lines(summary$sdeffect[summary$meffect==0.06], summary$pow_rq.t95[summary$meffect==0.06], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#268F0C",lwd=2)

lines(x=seq(0,0.8,0.1), y=rep(0.05,9), lwd=1.5, lty=2)

######
#Skewd
######
#no mean effect
plot(summary2$sdeffect[summary2$meffect==0],summary2$pow_lreg[summary2$meffect==0], 
     xlab= "Effect on sd", ylab= "Power", type="l", ylim=c(0,1), col="red", lwd=2,
     main="Power on regression models: Type A")

lines(summary2$sdeffect[summary2$meffect==0], summary2$pow_rq.t50[summary2$meffect==0], 
      xlab= "sdeffect", ylab= "tau.50 power", type="l", col= "#4E0C8F",lwd=2)

lines(summary2$sdeffect[summary2$meffect==0], summary2$pow_rq.t75[summary2$meffect==0], 
      xlab= "sdeffect", ylab= "tau.75 power",type="l", col="#105C9B",lwd=2)q

lines(summary2$sdeffect[summary2$meffect==0], summary2$pow_rq.t90[summary2$meffect==0], 
      xlab= "sdeffect", ylab= "tau.90 power", col="#62A7DF",lwd=2)

lines(summary2$sdeffect[summary2$meffect==0], summary2$pow_rq.t95[summary2$meffect==0], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#268F0C",lwd=2)

lines(summary2$sdeffect[summary2$meffect==0], summary2$pow_rq.t95[summary2$meffect==0], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#268F0C",lwd=2)

lines(x=seq(0,0.8,0.1), y=rep(0.05,9), lwd=1.5, lty=2)



#0.2 mean effect
plot(summary2$sdeffect[summary2$meffect==0.02],summary2$pow_lreg[summary2$meffect==0.02], 
     xlab= "Effect on sd", ylab= "Power", type="l", ylim=c(0,1), col="red", lwd=2,
     main="Power on regression models: meffect = 0, sdeffect= x, skewd")

lines(summary2$sdeffect[summary2$meffect==0.2], summary2$pow_rq.t50[summary2$meffect==0], 
      xlab= "sdeffect", ylab= "tau.50 power", type="l", col= "#4E0C8F",lwd=2)

lines(summary2$sdeffect[summary2$meffect==0], summary2$pow_rq.t75[summary2$meffect==0], 
      xlab= "sdeffect", ylab= "tau.75 power",type="l", col="#105C9B",lwd=2)

lines(summary2$sdeffect[summary2$meffect==0], summary2$pow_rq.t90[summary2$meffect==0], 
      xlab= "sdeffect", ylab= "tau.90 power", col="#62A7DF",lwd=2)

lines(summary2$sdeffect[summary2$meffect==0], summary2$pow_rq.t95[summary2$meffect==0], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#268F0C",lwd=2)

lines(summary2$sdeffect[summary2$meffect==0], summary2$pow_rq.t95[summary2$meffect==0], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#268F0C",lwd=2)

lines(x=seq(0,0.8,0.1), y=rep(0.05,9), lwd=1.5, lty=2)



#Get power of n5000 m=0.2 with quail
sim3_1 = subset(simulation, meffect==0.2)
summary_models= data.frame(matrix(ncol=9,nrow=0))
colnames(summary_models) = c("meffect", "sdeffect","skew","pow_lreg","pow_rq.t50",
                             "pow_rq.t75","pow_rq.t90","pow_rq.t95","pow_quail")
nsim=5000
sk=0
m= 0.2
for(sd in unique(sim3_1$sdeffect)){
this_model= subset(sim3_1, simulation$sdeffect==sd & simulation$meffect==0.2)
summary_models[nrow(summary_models)+1,]= c(m , sd, sk, 
                                           length(which(this_model$lreg_pv < 0.05))/nsim,
                                           length(which(this_model$rq_pval.t50 < 0.05))/nsim,
                                           length(which(this_model$rq_pval.t75 < 0.05))/nsim,
                                           length(which(this_model$rq_pval.t90 < 0.05))/nsim,
                                           length(which(this_model$rq_pval.t95 < 0.05))/nsim,
                                           length(which(this_model$quail_pval  < 0.05))/nsim)
write.csv(summary_models, file = "./summary_n5000nots.csv", sep = ",", append = T,row.names = F,col.names =!file.exists("./summary_n5000nots.csv") )
}


plot(summary3$sdeffect[summary3$meffect==0.2],summary3$pow_lreg[summary3$meffect==0.2], 
     xlab= "Effect on sd", ylab= "Power", type="l", ylim=c(0,1), xlim = c(0,0.3), col="red", lwd=2,
     main="Power on regression models: meffect = 0, sdeffect= x, skewd")

lines(summary3$sdeffect[summary3$meffect==0.2], summary3$pow_rq.t50[summary3$meffect==0.2], 
      xlab= "sdeffect", ylab= "tau.50 power", type="l", col= "#4E0C8F",lwd=2)

lines(summary3$sdeffect[summary3$meffect==0.2], summary3$pow_rq.t75[summary3$meffect==0.2], 
      xlab= "sdeffect", ylab= "tau.75 power",type="l", col="#105C9B",lwd=2)

lines(summary3$sdeffect[summary3$meffect==0.2], summary3$pow_rq.t90[summary3$meffect==0.2], 
      xlab= "sdeffect", ylab= "tau.90 power", col="#62A7DF",lwd=2)

lines(summary3$sdeffect[summary3$meffect==0.2], summary3$pow_rq.t95[summary3$meffect==0.2], 
      xlab= "sdeffect", ylab= "tau.95 power", col= "#268F0C",lwd=2)

lines(summary3$sdeffect[summary3$meffect==0.2], summary3$pow_quail[summary3$meffect==0.2], 
      xlab= "sdeffect", ylab= "quail power", col= "black",lwd=2)

lines(x=seq(0,0.8,0.1), y=rep(0.05,9), lwd=1.5, lty=2)
