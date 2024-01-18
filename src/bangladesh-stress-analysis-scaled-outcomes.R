
rm(list=ls())
source(here::here("0-config.R"))
#run using R version 4.0.3
#run using tmle version '1.3.0.2'

# #load stress outcomes dataset
d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS"))

#response to review statistics
compound <- read.csv("C:/Users/andre/Downloads/washb-bangladesh-track-compound-public.csv")
length(unique(compound$block))

#make oxidative stress sum score:
d$oxidative_stress_sum_score <- (scale(d$t2_f2_8ip) + scale(d$t2_f2_23d)+ scale(d$t2_f2_VI) + scale(d$t2_f2_12i))/4

outcomes <- c("oxidative_stress_sum_score",
              "t2_f2_8ip","t2_f2_23d","t2_f2_VI", "t2_f2_12i",
              "t3_map","t3_hr_mean",
              "t3_saa_z01","t3_saa_z02","t3_cort_z01","t3_cort_z03",
              "t3_gcr_mean","t3_gcr_cpg12","t3_saa_slope","t3_cort_slope","t3_residual_saa","t3_residual_cort")



#------------------------------------------------------------------------------------------------
# Unadjusted glm
#------------------------------------------------------------------------------------------------


#dataframe of stress outcomes:
colnames(d)



#Unadjusted glm models
res_unadj <- NULL
for(i in outcomes){
  print(i)
  #compare to GLM
  temp<-washb_glm(Y=(d[,i]), tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"))
  res_unadj<-rbind(res_unadj,  temp$TR)
  #temp<-washb_tmle(Y=scale(d[,i]), tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)
  #res_unadj<-rbind(res_unadj, unlist(temp$estimates$ATE))
}
res_unadj <- as.data.frame(res_unadj)
colnames(res_unadj)<-c("Mean difference","ci.l","ci.u","se","Zval", "Pval")
res_unadj$Y <- outcomes
rownames(res_unadj)=NULL

res_unadj %>% filter(Pval < 0.05)

res_unadj

#save for R01
saveRDS(res_unadj, file="C:/Users/andre/Documents/EE/washb_r01/figure_data/stress_figure_data_scaled.RDS")


