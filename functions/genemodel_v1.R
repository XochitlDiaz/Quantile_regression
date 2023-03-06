#Gene Models
#Jan 2023
#By Xochitl Diaz

source("./QUAIL_Sim_Fun.R")

#Before running 




#This function will generate samples of a given sample_size,
#the additive effect on the mean is specified by the meffect parameter
#the additive effect on sd or skewness is specified by the sdgeffect parameter
#the skew parameter indicates if genetic models will be generated from
# a normal distribution and use sdgeffect as effect on standard deviation, 
# or generated from a gamma distribution and use sdgeffect as effect on the
# k (shape) parameter, which is an indicator of skewness
run_model <- function(sample_size= 1000, meffect=0, sdgeffect=0, skew=F, p=0.5, q=0.5 ){
  #Assume HW equilibrium
  n0= round(sample_size*p*p)
  n1= round(sample_size*p*q)
  n2= round(sample_size*q*q)
  m0= 19
  m1= m0 * (meffect + 1)
  m2= m0 * (meffect*2 + 1)
  
  if (skew==F){
    s0= 5 
    s1= s0 * (sdgeffect + 1)
    s2= s0 * (sdgeffect*2 + 1)
    #Distribution model
    #Alt allele count=0
    Y0 = rnorm(n0, m0, s0)
    #Alt allele count=1
    Y1 = rnorm(n1, m1, s1)
    #Alt allele count=2
    Y2 = rnorm(n2, m2, s2)
  } else {
    #When skew=T a gamma distribution is generated
    gammadk0= 7
    gammadk1= gammadk0*(sdgeffect +1)
    gammadk2= gammadk0*(sdgeffect*2 +1)
    gammadtheta0= m0/gammadk0
    gammadtheta1= m1/gammadk1
    gammadtheta2= m2/gammadk2
    #Alt allele count=0
    Y0 = rgamma(n0, shape=gammadk0, scale = gammadtheta0)
    #Alt allele count=1
    Y1 = rgamma(n1, shape=gammadk1, scale = gammadtheta1)
    #Alt allele count=2
    Y2 = rgamma(n2, shape=gammadk2, scale = gammadtheta2)
  }
  
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
  lreg_beta= summary(lreg)$coefficients[2,1]
  #Quantile Regression 
  fit_rq = list('t25'= rq(trait ~ allelecount, tau = 0.25, data = Y),
                't50'= rq(trait ~ allelecount, tau = 0.5, data = Y),
                't75'= rq(trait ~ allelecount, tau = 0.75, data = Y),
                't90'= rq(trait ~ allelecount, tau = 0.9, data = Y),
                't95'= rq(trait ~ allelecount, tau = 0.95, data = Y))
  rq_pval= c(summary(fit_rq$t75, se='boot', bsmethod='xy')$coefficients[2,4],
             summary(fit_rq$t90, se='boot', bsmethod='xy')$coefficients[2,4],
             summary(fit_rq$t95, se='boot', bsmethod='xy')$coefficients[2,4])
  rq_beta = c(summary(fit_rq$t50, se='boot', bsmethod='xy')$coefficients[2,1],
              summary(fit_rq$t75, se='boot', bsmethod='xy')$coefficients[2,1],
              summary(fit_rq$t90, se='boot', bsmethod='xy')$coefficients[2,1],
              summary(fit_rq$t95, se='boot', bsmethod='xy')$coefficients[2,1])
  #QUAIL
  #returns pvalue of  
  quail = QUAIL(Y$allelecount, Y$trait, quantiles = 100)
  
  values= list('lreg_pv'=     summary(lreg)$coefficients[2,4],
               'lreg_beta'=   summary(lreg)$coefficients[2,1],
               'rq_pval.t25'= summary(fit_rq$t25, se='boot', bsmethod='xy')$coefficients[2,4],
               'rq_beta.t25'= summary(fit_rq$t25, se='boot', bsmethod='xy')$coefficients[2,1],
               'rq_pval.t50'= summary(fit_rq$t50, se='boot', bsmethod='xy')$coefficients[2,4],
               'rq_beta.t50'= summary(fit_rq$t50, se='boot', bsmethod='xy')$coefficients[2,1],
               'rq_pval.t75'= summary(fit_rq$t75, se='boot', bsmethod='xy')$coefficients[2,4],
               'rq_beta.t75'= summary(fit_rq$t75, se='boot', bsmethod='xy')$coefficients[2,1],
               'rq_pval.t90'= summary(fit_rq$t90, se='boot', bsmethod='xy')$coefficients[2,4],
               'rq_beta.t90'= summary(fit_rq$t90, se='boot', bsmethod='xy')$coefficients[2,1],
               'rq_pval.t95'= summary(fit_rq$t95, se='boot', bsmethod='xy')$coefficients[2,4],
               'rq_beta.t95'= summary(fit_rq$t95, se='boot', bsmethod='xy')$coefficients[2,1],
               'quail_pval'= quail$p,
               'quail_beta'= quail$beta)
  return(values)
}

