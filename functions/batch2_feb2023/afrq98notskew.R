#Feb 2023
#By Xochitl Diaz
source("./genemodel.R")



#####################
### n1000 !skewd - allelic frq = 98: ###
#####################


#Set Output file name:
outputfile = "./simulations_afrq98_notskew_table.csv"
summaryfile = "./summary_afreq98_notskew.csv"

sdproportion = seq(from = 0, to= 3, by = 0.2)
mproportion= seq(from = 0, to= 3, by=0.2)

gene_models= data.frame()

summary_models= data.frame(matrix(ncol=8,nrow=0))
colnames(summary_models) = c("meffect", "sdeffect","skew","pow_lreg","pow_rq.t50",
                             "pow_rq.t75","pow_rq.t95","pow_quail")
nsim=1000

sk= 0

for( m in mproportion){
  for (sd in sdproportion) {
    for (j in 1:nsim) {
      data = run_model(meffect=m, sdeffect=sd, skew=sk, p = 0.98)
      if (is.list(data)){
        data= as.data.frame(data)
        data$meffect=rep(m, nrow(data))
        data$sdeffect=rep(sd, nrow(data))
        data$skew= rep(sk,nrow(data))
        gene_models=rbind(gene_models,data)
        write.csv(gene_models, file = outputfile, append=T,
                  sep = ",", col.names = !file.exists(outputfile), row.names = F)
      }
    }
    this_model= subset(gene_models,gene_models$sdeffect==sd & gene_models$meffect==m)
    summary_models[nrow(summary_models)+1,]= c(m , sd, sk, 
                                               length(which(this_model$lreg_pv < 0.05))/nsim,
                                               length(which(this_model$rq_pval.t50 < 0.05))/nsim,
                                               length(which(this_model$rq_pval.t75 < 0.05))/nsim,
                                               length(which(this_model$rq_pval.t95 < 0.05))/nsim,
                                               length(which(this_model$quail_pval  < 0.05))/nsim)
    write.csv(summary_models, file = summaryfile, sep = ",", append = T,row.names = F,col.names =!file.exists(summaryfile) )
  }
}

