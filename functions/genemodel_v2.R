#Gene Models
#Jan 2023
#By Xochitl Diaz

#source("./QUAIL_Sim_Fun.R")
library(quantreg)
library(sn)
#Before running 

#QUAIL function from git hub
#https://github.com/qlu-lab/QUAIL
QUAIL <- function(x, y, quantiles = 100){
  x <- (x - mean(x))/sd(x)
  y <- lm(y~x)$residuals
  a_i_tau_list <- rep()
  k <- quantiles - 1 ### number of quantiles to fit the model
  d <- rnorm(length(x))
  for (i in 1:k){
    # print(i)
    tau_curr <- i/(k+1)
    Qreg <- rq(y ~ d, tau = tau_curr, method = "fn")
    SE_d_cur <- summary(Qreg , se = "ker")$coefficients[2,2]
    balance <-   sqrt(-tau_curr^2 + tau_curr)/SE_d_cur    
    a_i_tau <- ifelse(residuals(Qreg) < 0 ,1,0)
    a_i_tau_list[[i]] <- (-a_i_tau + tau_curr)/balance
  }
  
  int_rank_score <- data.frame(score = rep(0, length(y)))
  for (i in 1:(k-1)){
    if ( i > quantiles/2){
      term2 <- 1
    }else {
      term2 <- -1
    }
    int_rank_score <- int_rank_score + term2*a_i_tau_list[[i]]
  }
  
  int_rank_score <- int_rank_score/(quantiles/2)
  Xstar <-  lm(x ~ 1)$residuals
  null_phi_1 <- int_rank_score$score*sqrt(sum(Xstar^2))
  quail_val = list('p'= summary(lm(null_phi_1 ~ Xstar))$coefficients[2,4],
                   'beta'= summary(lm(null_phi_1 ~ Xstar))$coefficients[2,1])
  return(quail_val)
}


#This function will generate samples of a given sample_size,
#the additive effect on the mean is specified by the meffect parameter
#the additive effect on sd or skewness is specified by the sdgeffect parameter
#the skew parameter indicates if genetic models will be generated from
# a normal distribution and use sdgeffect as effect on standard deviation, 
# or generated from a gamma distribution and use sdgeffect as effect on the
# k (shape) parameter, which is an indicator of skewness
run_model <- function(sample_size= 1000, meffect=0, sdeffect=0, skew= 0, p=0.5 ){
  q= 1- p
  #Assume HW equilibrium
  n0= round(sample_size*p*p)
  n1= round(sample_size*2*p*q)
  n2= round(sample_size*q*q)
  m0= 19
  m1= m0 * (meffect + 1)
  m2= m0 * (meffect*2 + 1)
  sd0= 5 
  sd1= sd0 * (sdeffect + 1)
  sd2= sd0 * (sdeffect*2 + 1)
  sk0= skew
  sk1= skew
  sk2= skew
  #Distribution model
  #Alt allele count=0
  Y0 = rsn(n0, m0, sd0, sk0)
  #Alt allele count=1
  Y1 = rsn(n1, m1, sd1, sk1)
  #Alt allele count=2
  Y2 = rsn(n2, m2, sd2 ,sk2)
  #All data
  Y =c(Y0,Y1,Y2)
  #plot(density(Y))
  if ((range(Y2)[1]>range(Y1)[2]) && (range(Y1[1])> range(Y0[2]))){
    return(NA)
  }
  Y= data.frame(trait= Y,
                allelecount= c(rep(0, n0),rep(1,n1),rep(2,n2)))
  means = tapply(Y$trait, Y$allelecount, mean)
  #boxplot(trait~allelecount, data = Y,
  #        xlab = "Allele Count", ylab = "Trait Units",
  #        col=c("#dbbc4b", "#dba14b","#db804b"), ylim= c(0,70))
  #points(means, pch=20, col="red")
  
  values = run_regressions(Y)
  return(values)
}




run_regressions <- function(Y){
  #Linear regression
  lreg=lm(trait ~ allelecount, Y)
  #Quantile Regression 
  fit_rq = list('t50'= rq(trait ~ allelecount, tau = 0.5, data = Y),
                't75'= rq(trait ~ allelecount, tau = 0.75, data = Y),
                't95'= rq(trait ~ allelecount, tau = 0.95, data = Y),
                't99'= rq(trait ~ allelecount, tau = 0.99, data = Y))
  rq_pval= c(summary(fit_rq$t50, se='boot', bsmethod='xy')$coefficients[2,4],
             summary(fit_rq$t75, se='boot', bsmethod='xy')$coefficients[2,4],
             summary(fit_rq$t95, se='boot', bsmethod='xy')$coefficients[2,4],
             summary(fit_rq$t99, se='boot', bsmethod='xy')$coefficients[2,4])
  rq_beta = c(summary(fit_rq$t50, se='boot', bsmethod='xy')$coefficients[2,1],
              summary(fit_rq$t75, se='boot', bsmethod='xy')$coefficients[2,1],
              summary(fit_rq$t95, se='boot', bsmethod='xy')$coefficients[2,1],
              summary(fit_rq$t99, se='boot', bsmethod='xy')$coefficients[2,1])
  #QUAIL
  #returns pvalue of and beta
  quail = QUAIL(Y$allelecount, Y$trait, quantiles = 100)
  
  values= list('lreg_pv'=     summary(lreg)$coefficients[2,4],
               'lreg_beta'=   summary(lreg)$coefficients[2,1],
               'rq_pval.t50'= summary(fit_rq$t50, se='boot', bsmethod='xy')$coefficients[2,4],
               'rq_beta.t50'= summary(fit_rq$t50, se='boot', bsmethod='xy')$coefficients[2,1],
               'rq_pval.t75'= summary(fit_rq$t75, se='boot', bsmethod='xy')$coefficients[2,4],
               'rq_beta.t75'= summary(fit_rq$t75, se='boot', bsmethod='xy')$coefficients[2,1],
               'rq_pval.t95'= summary(fit_rq$t95, se='boot', bsmethod='xy')$coefficients[2,4],
               'rq_beta.t95'= summary(fit_rq$t95, se='boot', bsmethod='xy')$coefficients[2,1],
               'rq_pval.t99'= summary(fit_rq$t99, se='boot', bsmethod='xy')$coefficients[2,4],
               'rq_beta.t99'= summary(fit_rq$t99, se='boot', bsmethod='xy')$coefficients[2,1],
               'quail_pval'= quail$p,
               'quail_beta'= quail$beta)
  return(values)
}

