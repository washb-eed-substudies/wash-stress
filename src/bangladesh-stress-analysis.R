
rm(list=ls())
source(here::here("0-config.R"))
#run using R version 4.0.3
#run using tmle version '1.3.0.2'

#load the fake dataset
# d <- readRDS("~/ee-secondary/replication objects/simulated_stress_dataset.rds")
# head(d)

# #load stress outcomes dataset
d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS"))

#response to review statistics
compound <- read.csv("C:/Users/andre/Downloads/washb-bangladesh-track-compound-public.csv")
length(unique(compound$block))



#------------------------------------------------------------------------------------------------
# Age stats
#------------------------------------------------------------------------------------------------


stress_age2_overall <- d %>% filter(!is.na(ur_agem2)) %>%
  summarize(tr="Overall",N=n(), mean=mean(ur_agem2), median=median(ur_agem2), sd=sd(ur_agem2), female=sum(sex==0), male=sum(sex))
stress_age2_tr <- d %>% group_by(tr) %>% filter(!is.na(ur_agem2)) %>%
  summarize(N=n(), mean=mean(ur_agem2), median=median(ur_agem2), sd=sd(ur_agem2), female=sum(sex==0), male=sum(sex))
stress_age_t2_M <- rbind(stress_age2_overall, stress_age2_tr)

stress_age3_overall <- d %>% filter(!is.na(agemth_t3_oragene)) %>%
  summarize(tr="Overall",N=n(), mean=mean(agemth_t3_oragene), median=median(agemth_t3_oragene), sd=sd(agemth_t3_oragene), female=sum(sex==0), male=sum(sex))
stress_age3_tr <- d %>% group_by(tr) %>% filter(!is.na(agemth_t3_oragene)) %>%
  summarize(N=n(), mean=mean(agemth_t3_oragene), median=median(agemth_t3_oragene), sd=sd(agemth_t3_oragene), female=sum(sex==0), male=sum(sex))
stress_age_t3_M <- rbind(stress_age3_overall, stress_age3_tr)


# # #Compare to Audrie's
# load(here::here("audrie results/stress-age-stats.RData"))
# stress_age_t2_L[,-1] - stress_age_t2_M[,-1]
# stress_age_t3_L[,-1] - stress_age_t3_M[,-1]


#------------------------------------------------------------------------------------------------
# N's and absolute means
#------------------------------------------------------------------------------------------------

raw_outcomes <- c("t2_f2_8ip_raw","t2_f2_23d_raw","t2_f2_VI_raw", "t2_f2_12i_raw",
                  "t3_map","t3_hr_mean",
                  "t3_saa_z01_raw","t3_saa_z02_raw","t3_cort_z01_raw","t3_cort_z03_raw",
                  "t3_gcr_mean_raw","t3_gcr_cpg12_raw","t3_saa_slope","t3_cort_slope","t3_residual_saa","t3_residual_cort")

#Function to count non-missing outcomes
N_y <- function(x, na.rm=T){ sum(!is.na(x),na.rm=na.rm)}

absolute_mean_sd <- d %>% filter(tr %in% c("Nutrition + WSH", "Control")) %>% subset(., select=c(raw_outcomes)) %>% 
  summarise_all(tibble::lst(mean, sd, N_y), na.rm=T) %>% 
  gather() %>% as.data.frame()
n <-nrow(absolute_mean_sd)/3
#split mean and SD into different columns
absolute_mean_sd <- data.frame(Y=gsub("_mean","",absolute_mean_sd[1:n,1]), mean=absolute_mean_sd[1:n,2], sd=absolute_mean_sd[(n+1):(2*n),2],  n=absolute_mean_sd[(2*n+1):(3*n),2]) 

#Mean and SD by treatment arm
absolute_mean_sd_tr <- d %>% group_by(tr) %>%
  subset(., select=c("tr",raw_outcomes))%>% 
  summarise_all(tibble::lst(mean, sd, N_y), na.rm=T) %>% 
  gather(.,  stat, measurement, t2_f2_8ip_raw_mean:t3_residual_cort_N_y, factor_key=TRUE) %>%
  as.data.frame()


#Mean and SD by sex
absolute_mean_sd_sex <- d %>% filter(tr %in% c("Nutrition + WSH", "Control")) %>% group_by(sex) %>%
  subset(., select=c("sex",raw_outcomes))%>% 
  summarise_all(tibble::lst(mean, sd, N_y), na.rm=T) %>% 
  gather(.,  stat, measurement, t2_f2_8ip_raw_mean:t3_residual_cort_N_y, factor_key=TRUE) %>%
  as.data.frame()


n <-nrow(absolute_mean_sd_tr)/3
#split mean and SD into different columns
absolute_mean_sd_tr <- data.frame(tr=absolute_mean_sd_tr[1:n,1], Y=gsub("_mean","",absolute_mean_sd_tr[1:n,2]), mean=absolute_mean_sd_tr[1:n,3], sd=absolute_mean_sd_tr[(n+1):(2*n),3],  n=absolute_mean_sd_tr[(2*n+1):(3*n),3]) 

n<-nrow(absolute_mean_sd_sex)/3
absolute_mean_sd_sex <- data.frame(sex=absolute_mean_sd_sex[1:n,1], Y=gsub("_mean","",absolute_mean_sd_sex[1:n,2]), mean=absolute_mean_sd_sex[1:n,3], sd=absolute_mean_sd_sex[(n+1):(2*n),3],  n=absolute_mean_sd_sex[(2*n+1):(3*n),3]) 



#------------------------------------------------------------------------------------------------
# N's and transformed means
#------------------------------------------------------------------------------------------------



outcomes <- c("t2_f2_8ip","t2_f2_23d","t2_f2_VI", "t2_f2_12i",
              "t3_map","t3_hr_mean",
              "t3_saa_z01","t3_saa_z02","t3_cort_z01","t3_cort_z03",
              "t3_gcr_mean","t3_gcr_cpg12","t3_saa_slope","t3_cort_slope","t3_residual_saa","t3_residual_cort")

mean_sd <- d %>% subset(., select=c(outcomes)) %>% 
  summarise_all(tibble::lst(mean, sd, N_y), na.rm=T) %>% 
  gather() %>% as.data.frame()
n <-nrow(mean_sd)/3
#split mean and SD into different columns
mean_sd <- data.frame(Y=gsub("_mean","",mean_sd[1:n,1]), mean=mean_sd[1:n,2], sd=mean_sd[(n+1):(2*n),2],  n=mean_sd[(2*n+1):(3*n),2]) 

#Mean and SD by treatment arm
mean_sd_tr <- d %>% group_by(tr) %>% subset(., select=c("tr",outcomes))%>% 
  summarise_all(tibble::lst(mean, sd, N_y), na.rm=T) %>% 
  gather(.,  stat, measurement, t2_f2_8ip_mean:t3_residual_cort_N_y, factor_key=TRUE) %>%
  as.data.frame()

n <-nrow(mean_sd_tr)/3
#split mean and SD into different columns
mean_sd_tr <- data.frame(tr=mean_sd_tr[1:n,1], Y=gsub("_mean","",mean_sd_tr[1:n,2]), mean=mean_sd_tr[1:n,3], sd=mean_sd_tr[(n+1):(2*n),3],  n=mean_sd_tr[(2*n+1):(3*n),3]) 


# #Compare to Audrie's
# load(here::here("audrie results/stress_N_means.RData"))
# ls()
# #
# aud_N <- as.data.frame(rbindlist(lapply(ls(pattern="_N_L"), get)))
# aud_N$Y = gsub("_N_L","",ls(pattern="_N_L"))
# #
# # #merge and compare
# N_comp <- merge(aud_N, mean_sd, by="Y")
# dim(N_comp)
# N_comp$mean.diff <- N_comp$mean.x - N_comp$mean.y
# N_comp$sd.diff <- N_comp$sd.x - N_comp$sd.y
# max(N_comp$mean.diff)
# max(N_comp$sd.diff)




#------------------------------------------------------------------------------------------------
# transformed means with 95% CI's for figure
#------------------------------------------------------------------------------------------------

washb_mean_y <- function(x, y="block"){washb_mean(Y=x, id=y)}


#Mean and 95%CI by treatment arm
mean_ci_tr<-NULL
for(i in outcomes){
  wshn <- d %>% filter(tr=="Nutrition + WSH") %>% 
    subset(., select=c("block",i)) %>% as.data.frame() %>%
    do(as.data.frame(washb_mean(Y=.[,2], id=.$block,  print=F)))
  control <- d %>% filter(tr=="Control") %>% 
    subset(., select=c("block",i)) %>% as.data.frame() %>%
    do(as.data.frame(washb_mean(Y=.[,2], id=.$block,  print=F)))
  mean_ci_tr<-bind_rows(
    mean_ci_tr,
    data.frame(outcome=i, tr="Nutrition + WSH", control),
    data.frame(outcome=i, tr="Control", wshn))
}



#------------------------------------------------------------------------------------------------
# Unadjusted tmle
#------------------------------------------------------------------------------------------------


#dataframe of stress outcomes:
colnames(d)

for(i in outcomes){
  print(table(is.na(d[,i])))
}

#Unadjusted glm models
res_unadj <- NULL
for(i in outcomes){
  print(i)
  #compare to GLM
  #temp<-washb_glm(Y=(d[,i]), tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"))
  temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)
  res_unadj<-rbind(res_unadj, unlist(temp$estimates$ATE))
}
res_unadj <- as.data.frame(res_unadj)
res_unadj

colnames(res_unadj)<-c("Mean difference","var","ci.l","ci.u", "Pval")
res_unadj$Y <- outcomes
res_unadj

vitals_unadj <- NULL
for(i in c("t3_hr_mean", "t3_map")){
  for (arm in c("Nutrition + WSH")){
    print(i)
    temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control",arm), print=F, seed=12345)
    vitals_unadj<-rbind(vitals_unadj, unlist(temp$estimates$ATE))
  }
}

vitals_unadj <- as.data.frame(vitals_unadj) %>% mutate(tr = rep(c("Nutrition + WSH"), 2),
                                                 Y = c("t3_hr_mean",  "t3_map"))
colnames(vitals_unadj)<-c("Mean difference","var","ci.l","ci.u", "Pval","tr","Y")
vitals_unadj

# i="t3_gcr_cpg12_raw"
# table(is.na(d$t3_gcr_cpg12))
# d <- d %>% filter(!is.na(t3_gcr_cpg12_raw))
# temp<-washb_tmle(Y=1*(d[,i]>0), tr=d$tr, W=NULL, id=d$block, pair=NULL, family="binomial", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)


# # #Compare to Audrie's objects
# load(here::here("audrie results/stress_unadj_glm.RData"))
# 
# name.pattern="unadj_"
# object_list=ls(pattern=name.pattern)
# aud_unadj <- rbind(get(object_list[1]), get(object_list[2]))
# names(aud_unadj)[names(aud_unadj) == 'var'] <- "Y"
# 
# #
# dim(res_unadj)
# dim(aud_unadj)
# comp_unadj <- full_join(res_unadj, aud_unadj, by="Y")
# dim(comp_unadj)
# #
# comp_unadj$`Mean difference` - comp_unadj$RD
# comp_unadj$`P-value` - comp_unadj$Pval
# 
# saveRDS(d, here::here("replication objects/andrew_stress_object.RDS"))



#------------------------------------------------------------------------------------------------
# Age and sex adjusted GLMs
#------------------------------------------------------------------------------------------------

d$sex<-as.factor(d$sex)
d$sex=relevel(factor(d$sex),ref="0")

#Age and sex adjusted glm models - set pval to .99 to avoid prescreening
res_sex <- NULL
for(i in outcomes){
  if(grepl("t2_", i)){
    temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=data.frame(sex=d$sex, age=d$ur_agem2), id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, pval = 1)
  }else{
    if(i %in% c("t3_map","t3_hr_mean" )){
      temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=data.frame(sex=d$sex, age=d$vital_aged3), id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, pval = 1)
    }
    if(grepl("saa|cort", i)){
      temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=data.frame(sex=d$sex, age=d$salimetrics_aged3), id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, pval = 1)
    }
    if(i %in% c("t3_gcr_mean", "t3_gcr_cpg12")){
      temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=data.frame(sex=d$sex, agem2=d$oragene_aged3), id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, pval = 1)
    }
  }
  res_sex<-rbind(res_sex, unlist(temp$estimates$ATE))
}
res_sex <- as.data.frame(res_sex)

colnames(res_sex)<-c("Mean difference","var","ci.l","ci.u", "Pval")
res_sex$Y <- outcomes


vitals_age_sex_adj <- NULL
for(i in c("t3_hr_mean", "t3_map")){
  for (arm in c("Nutrition + WSH")){
    print(i)
    temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=data.frame(sex=d$sex, age=d$vital_aged3), id=d$block, pair=NULL, family="gaussian", contrast= c("Control",arm), print=F, pval = 1)
    vitals_age_sex_adj<-rbind(vitals_age_sex_adj, unlist(temp$estimates$ATE))
  }
}

vitals_age_sex_adj <- as.data.frame(vitals_age_sex_adj) %>% mutate(tr = rep(c("Nutrition + WSH"), 2),
                                                       Y = c("t3_hr_mean", "t3_map"))
colnames(vitals_age_sex_adj)<-c("Mean difference","var","ci.l","ci.u", "Pval","tr","Y")
vitals_age_sex_adj

# #Compare to Audrie's objects
# load(here("audrie results/immune_adj_sex_age_glm.RData"))
# 
# aud_sex <- as.data.frame(rbindlist(lapply(lapply(ls(pattern="_adj_sex_age_L"), get), as.data.frame)))
# aud_sex$Y = gsub("_adj_sex_age_L","",ls(pattern="_adj_sex_age_L"))
# 
# dim(res_sex)
# dim(aud_sex)
# comp_sex <- full_join(res_sex, aud_sex, by="Y")
# dim(comp_sex)
# 
# comp_sex$RD.x - comp_sex$RD.y


#------------------
# Clean adjustment variables
# Note: no need ot change this code
#------------------

#Make vectors of adjustment variable names
Wvars<-c('sex', 'birthord',
         'momage', 'momheight','momedu','hfiacat',
         'Nlt18','Ncomp','watmin',
         'wall', 'floor',
         'elec', 'asset_wardrobe', 'asset_table', 'asset_chair', 'asset_clock', 
         'asset_khat', 'asset_chouki', 'asset_radio', 
         'asset_tv', 'asset_refrig', 'asset_bike',
         'asset_moto', 'asset_sewmach', 'asset_mobile',
         'n_cattle', 'n_goat', 'n_chicken')



#Add in time varying covariates:
Wvars2<-c("ageday_ut2", "monsoon_ut2") 
Wvars3_vital<-c("vital_aged3", "monsoon_t3_vital") 
Wvars3_salimetrics<-c("salimetrics_aged3", "monsoon_t3_salimetrics") 
Wvars3_salimetrics_time<-c("salimetrics_aged3", "monsoon_t3_salimetrics", "t3_col_time_z01_cont")
Wvars3_oragene<-c("oragene_aged3", "monsoon_t3_oragene") 



#subset time-constant W adjustment set
Wvars[!(Wvars %in% colnames(d))]
W <- subset(d, select=Wvars)

#Clean adjustment variables 
#Check missingness
for(i in 1:ncol(W)){
  print(colnames(W)[i])
  print(table(is.na(W[,i])))
}

#Replace missingness for factors with new level
#in main dataset 

d$asset_clock <- as.character(d$asset_clock)
d$asset_clock[is.na(d$asset_clock)]<-99
d$asset_clock<-factor(d$asset_clock)

#Order data to replicate SL
d <- d[order(d$dataid, d$childid, d$svy),]



#Re-subset W so new missing categories are included
W<- subset(d, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  print(colnames(W)[i])
  print(class(W[,i])  )
}

#Truncate unrealistic levels of n_chickens to 60
table(d$n_chicken)
d$n_chickens[d$n_chicken>60]<-60
table(d$n_chicken)

#Relevel all factors
d$sex=relevel(factor(d$sex),ref="0")
d$momedu=relevel(factor(d$momedu),ref="No education")
d$hfiacat=relevel(factor(d$hfiacat),ref="Food Secure")
d$hfiacat<-addNA(d$hfiacat)
d$wall<-factor(d$wall)
d$wall<-addNA(d$wall)
levels(d$wall)<-c("No improved wall","Improved wall","Missing")
d$wall=relevel(factor(d$wall),ref="No improved wall")
d$floor<-factor(d$floor)
d$floor<-addNA(d$floor)
levels(d$floor)<-c("No improved floor","Improved floor","Missing")
d$floor=relevel(d$floor,ref="No improved floor")
d$elec<-factor(d$elec)
d$elec<-addNA(d$elec)
levels(d$elec)<-c("No electricity","Electricity","Missing")
d$elec=relevel(factor(d$elec),ref="No electricity")
d$asset_wardrobe<-factor(d$asset_wardrobe)
d$asset_wardrobe<-addNA(d$asset_wardrobe)
levels(d$asset_wardrobe)<-c("No wardrobe","Wardrobe","Missing")
d$asset_wardrobe=relevel(factor(d$asset_wardrobe),ref="No wardrobe")
d$asset_table<-factor(d$asset_table)
d$asset_table<-addNA(d$asset_table)
levels(d$asset_table)<-c("No table","Improved table","Missing")
d$asset_table=relevel(factor(d$asset_table),ref="No table")
d$asset_chair<-factor(d$asset_chair)
d$asset_chair<-addNA(d$asset_chair)
levels(d$asset_chair)<-c("No chair","Chair","Missing")
d$asset_chair=relevel(factor(d$asset_chair),ref="No chair")
d$asset_clock<-factor(d$asset_clock)
# d$asset_clock=relevel(factor(d$asset_clock),ref="No clock")
levels(d$asset_clock)<-c("No clock","Clock","Missing")
d$asset_clock=relevel(factor(d$asset_clock),ref="No clock")
d$asset_khat<-factor(d$asset_khat)
d$asset_khat<-addNA(d$asset_khat)
levels(d$asset_khat)<-c("No khat","Khat","Missing")
d$asset_khat=relevel(factor(d$asset_khat),ref="No khat")
d$asset_chouki<-factor(d$asset_chouki)
d$asset_chouki<-addNA(d$asset_chouki)
levels(d$asset_chouki)<-c("No chouki","Chouki","Missing")
d$asset_chouki=relevel(factor(d$asset_chouki),ref="No chouki")
d$asset_tv<-factor(d$asset_tv)
d$asset_tv<-addNA(d$asset_tv)
levels(d$asset_tv)<-c("No TV","Improved TV","Missing")
d$asset_tv=relevel(factor(d$asset_tv),ref="No TV")
d$asset_refrig<-factor(d$asset_refrig)
d$asset_refrig<-addNA(d$asset_refrig)
levels(d$asset_refrig)<-c("No refrigerator","Refrigerator","Missing")
d$asset_refrig=relevel(factor(d$asset_refrig),ref="No refrigerator")
d$asset_bike<-factor(d$asset_bike)
d$asset_bike<-addNA(d$asset_bike)
levels(d$asset_bike)<-c("No bicycle","Bicycle","Missing")
d$asset_bike=relevel(factor(d$asset_bike),ref="No bicycle")
d$asset_moto<-factor(d$asset_moto)
d$asset_moto<-addNA(d$asset_moto)
levels(d$asset_moto)<-c("No motorcycle","Motorcycle","Missing")
d$asset_moto=relevel(factor(d$asset_moto),ref="No motorcycle")
d$asset_sewmach<-factor(d$asset_sewmach)
d$asset_sewmach<-addNA(d$asset_sewmach)
levels(d$asset_sewmach)<-c("No sewing machine","Sewing machine","Missing")
d$asset_sewmach=relevel(factor(d$asset_sewmach),ref="No sewing machine")
d$asset_mobile<-factor(d$asset_mobile)
d$asset_mobile<-addNA(d$asset_mobile)
levels(d$asset_mobile)<-c("No mobile phone","Mobile phone","Missing")
d$asset_mobile=relevel(factor(d$asset_mobile),ref="No mobile phone")    

#Re-subset W so new re-leveled factors are included
W<- subset(d, select=Wvars)



#Add in time-varying covariates
W2<- cbind(W, subset(d, select=Wvars2))
W3_vital<- cbind(W, subset(d, select=Wvars3_vital))
W3_salimetrics<- cbind(W, subset(d, select=Wvars3_salimetrics))
W3_salimetrics_time<-cbind(W, subset(d, select=Wvars3_salimetrics_time))
W3_oragene<- cbind(W, subset(d, select=Wvars3_oragene))





##############################################
#Run GLMs for the adjusted parameter estimates
##############################################

#Fully adjusted glm models
res_adj <- NULL
for(i in outcomes){
  if(grepl("t2_", i)){
    temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=W2, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)
  }else{
    if(i %in% c("t3_map","t3_hr_mean" )){
      temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=W3_vital, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)
    }
    if(i %in% c("t3_saa_z01","t3_saa_z02","t3_cort_z01","t3_cort_z03","t3_saa_slope","t3_cort_slope")){
      temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=W3_salimetrics_time, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)
    }
    if(i %in% c("t3_residual_saa",  "t3_residual_cort")){
      temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=W3_salimetrics_time, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)
    }
    if(i %in% c("t3_gcr_mean", "t3_gcr_cpg12")){
      temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=W3_oragene, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, seed=12345)
    }
  }
  res_adj<-rbind(res_adj, unlist(temp$estimates$ATE))
}
res_adj <- as.data.frame(res_adj)



colnames(res_adj)<-c("Mean difference","var","ci.l","ci.u", "Pval")
res_adj$Y <- outcomes


vitals_adj <- NULL
for(i in c("t3_hr_mean", "t3_map")){
  for (arm in c("Nutrition", "WSH")){
    print(i)
    print(arm)
    temp<-washb_tmle(Y=(d[,i]), tr=d$tr, W=W3_vital, id=d$block, pair=NULL, family="gaussian", contrast= c("Control",arm), print=F, seed=12345)
    vitals_adj<-rbind(vitals_adj, unlist(temp$estimates$ATE))
  }
}
temp$fit
vitals_adj <- as.data.frame(vitals_adj) %>% mutate(tr = rep(c("Nutrition", "WSH"), 2),
                                                                   Y = c("t3_hr_mean", "t3_hr_mean", "t3_map", "t3_map"))
colnames(vitals_adj)<-c("Mean difference","var","ci.l","ci.u", "Pval","tr","Y")
vitals_adj


# #Compare to Audrie's objects
# load(here("audrie results/immune_adj_glm.RData"))
# name.pattern="_adj_L"
# object_list=ls(pattern=name.pattern)
# aud_adj <- load_aud(name.pattern, object_list)
# 
# 
# dim(res_adj)
# dim(aud_adj)
# comp_adj <- full_join(res_adj, aud_adj, by="Y")
# dim(comp_adj)
# comp_adj$RD.x - comp_adj$RD.y
# comp_adj$Pval - comp_adj$P.value

#Save intermediate R objects for replication comparison
# dm <- d
# save(res_adj, W, W2, W3_vital, W3_salimetrics, W3_oragene, dm,  file = here("replication objects/lisa_stress_W.rdata"))


##############################################
#Run GLMs for the sex-stratified subgroup analysis
##############################################

#sex stratified glm models
res_sub <- NULL
for(i in outcomes){
  temp<-washb_glm(Y=(d[,i]), tr=d$tr, W=data.frame(sex=d$sex), V="sex", id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F, verbose = F)
  lincom <- temp$lincom
  fit <- temp$fit
  int.P <- fit[grepl("\\:",rownames(fit)),6]
  lincom$int.P <- int.P
  res_sub<-rbind(res_sub, lincom)
}
res_sub <- as.data.frame(res_sub)
res_sub


colnames(res_sub)<-c("sex","RD", "Std. Error", "ci.l","ci.u","z value", "Pval", "intP")
res_sub$Y <-rep(outcomes, each=2)
res_sub <- res_sub %>% mutate(subgroup = case_when(sex==1 ~ "male", sex==0 ~ "female", TRUE~""), subgroup=factor(subgroup))

res_sub

# #Compare to Audrie's objects
# load(here("audrie results/immune_subgroup.RData"))
# 
# name.pattern="_subgroup_L"
# object_list=ls(pattern=name.pattern)
# aud_sub <- load_aud(name.pattern, object_list, subgroup = T)
# 
# dim(res_sub)
# dim(aud_sub)
# comp_sub <- full_join(res_sub, aud_sub, by=c("Y","subgroup"))
# dim(comp_sub)
# 
# comp_sub <- filter(comp_sub, !is.na(RD.x))
# 
# comp_sub$RD.x - comp_sub$RD.y



##############################################
#Run GLMs for the sex-stratified subgroup analysis
##############################################

#save results
save(stress_age_t2_M, stress_age_t3_M, mean_sd, mean_sd_tr, mean_ci_tr, absolute_mean_sd, absolute_mean_sd_tr, absolute_mean_sd_sex,
     res_unadj, res_sex, res_adj, res_sub, file=here::here("results/stress_results.Rdata"))

save(vitals_age_sex_adj, vitals_unadj, vitals_adj, file=here('results/vitals_all_arms/vitals_results.Rdata'))

# save(stress_age_t2_M, stress_age_t3_M, mean_sd, mean_sd_tr, mean_ci_tr, absolute_mean_sd, absolute_mean_sd_tr, absolute_mean_sd_sex,
#      res_unadj, res_sex, res_adj, res_sub, file=here::here("results/stress_results_newcovariate.Rdata"))