#March 2023
#By Xochitl Diaz
source("./genemodel.R")



#####################
### n800 !skewd - allelic frq changing ###
#####################


#Set Output file name:
outputfile = "./simulations_afrqchangem0_table.csv"
summaryfile = "./summary_afrqchangem0.csv"

sdproportion = seq(from = 0, to= 1.5, by = 0.2)
mproportion= seq(from = 0, to= 1.5, by=0.2)

gene_models= data.frame()

summary_models= data.frame(matrix(ncol=9,nrow=0))
colnames(summary_models) = c("meffect", "sdeffect","skew","afreq","pow_lreg","pow_rq.t50",
                             "pow_rq.t75","pow_rq.t95","pow_quail")
nsim=1000

sk= 0

afrquencies = seq(0.50,0.99,0.01)
for( m in mproportion){
  for (sd in sdproportion) {
    for (afreq in afrquencies) {
      for (j in 1:nsim) {
        data = run_model(meffect=m, sdeffect=sd, skew=sk, p = afreq)
        if (is.list(data)){
          data= as.data.frame(data)
          data$meffect=rep(m, nrow(data))
          data$sdeffect=rep(sd, nrow(data))
          data$skew= rep(sk,nrow(data))
          data$afreq= rep(afreq,nrow(data))
          gene_models=rbind(gene_models,data)
          write.csv(gene_models, file = outputfile, append=T,
                    sep = ",", col.names = !file.exists(outputfile), row.names = F)
        }
      }
    
    this_model= subset(gene_models,gene_models$sdeffect==sd & gene_models$meffect==m, gene_models$afreq==afreq)
    summary_models[nrow(summary_models)+1,]= c(m , sd, sk, afreq,
                                               length(which(this_model$lreg_pv < 0.05))/nsim,
                                               length(which(this_model$rq_pval.t50 < 0.05))/nsim,
                                               length(which(this_model$rq_pval.t75 < 0.05))/nsim,
                                               length(which(this_model$rq_pval.t95 < 0.05))/nsim,
                                               length(which(this_model$quail_pval  < 0.05))/nsim)
    write.csv(summary_models, file = summaryfile, sep = ",", append = T,row.names = F,col.names =!file.exists(summaryfile) )
    }
  }
}

