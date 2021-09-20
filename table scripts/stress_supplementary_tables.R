rm(list=ls())
library("xtable")
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS"))
data_y1 <- read.csv(here("tables/main/stress_table1.csv"))
load(here("results/stress_results.RData"))
load(here("results/stress_ipcw.rdata"))



#### Table S1 ####
# filtering for children with any t2 measurements
filtering <- function(row){
  any(!is.na(row))
}

y1<-d[apply(select(d, grep("t2_f2", names(d), ignore.case=T)), 1, filtering),]

# filtering children with any t2 measurements for those lost to follow up at t3
lost<-y1 %>% filter_at(vars(t3_map,t3_hr_mean,t3_saa_z01,t3_saa_z02,t3_cort_z01,t3_cort_z03,
                            t3_gcr_mean,t3_gcr_cpg12,t3_saa_slope,t3_cort_slope,t3_residual_saa,t3_residual_cort), all_vars(is.na(.)))

#calculating overall N by arm
Nlostctrl<-nrow(lost[lost$tr=="Control",])
Nlostwsh<-nrow(lost[lost$tr=="Nutrition + WSH",])

#functions for calculating %/mean for all variables in table based on arm
meansdfunc <- function(tbl, variable) {
  ctrlmean<-round(mean(variable[tbl$tr=="Control"], na.rm=TRUE))
  ctrlsd<-round(sd(variable[tbl$tr=="Control"], na.rm=TRUE))
  wshmean<-round(mean(variable[tbl$tr=="Nutrition + WSH"], na.rm=TRUE))
  wshsd<-round(sd(variable[tbl$tr=="Nutrition + WSH"], na.rm=TRUE))
  c(ctrlmean, ctrlsd, wshmean, wshsd)
}

npercfunc <- function(tbl, variable) {
  ctrln<-sum(variable[tbl$tr=="Control"], na.rm=TRUE)
  ctrlperc<-round(mean(variable[tbl$tr=="Control"], na.rm=TRUE)*100)
  wshn<-sum(variable[tbl$tr=="Nutrition + WSH"], na.rm=TRUE)
  wshperc<-round(mean(variable[tbl$tr=="Nutrition + WSH"], na.rm=TRUE)*100)
  c(ctrln, ctrlperc, wshn, wshperc)
}

momage<-meansdfunc(lost, lost$momage)
momeduy<-meansdfunc(lost, lost$momeduy)
dadeduy<-meansdfunc(lost, lost$dadeduy)
dadagri<-npercfunc(lost, lost$dadagri)
Nhh<-meansdfunc(lost, lost$Nhh)
elec<-npercfunc(lost, lost$elec)
cement<-npercfunc(lost, lost$cement)

acresmctrl<-round(mean(lost$landacre[lost$tr=="Control"], na.rm=TRUE), 2)
acressdctrl<-round(sd(lost$landacre[lost$tr=="Control"], na.rm=TRUE), 2)
acresmwsh<-round(mean(lost$landacre[lost$tr=="Nutrition + WSH"], na.rm=TRUE), 2)
acressdwsh<-round(mean(lost$landacre[lost$tr=="Nutrition + WSH"], na.rm=TRUE), 2)
acres<-c(acresmctrl, acressdctrl, acresmwsh, acressdwsh)

tubewell<-npercfunc(lost, lost$tubewell)
storewater<-npercfunc(lost, lost$storewat)
treatwater<-npercfunc(lost, lost$treatwat)
waterdis<-meansdfunc(lost, lost$watmin)
odmen<-npercfunc(lost, lost$odmen)
odwomen<-npercfunc(lost, lost$odwom)
odchild815<-npercfunc(lost, lost$odch815)
odchild38<-npercfunc(lost, lost$odch38)
odchild03<-npercfunc(lost, lost$odchu3)
latowned<-npercfunc(lost, lost$latown)
latslab<-npercfunc(lost, lost$latslab)
latseal<-npercfunc(lost, lost$latseal)

latfctrln<-sum(lost$latfeces[lost$tr=="Control"]==0, na.rm=T)
latfctrlperc<-round(latfctrln/sum(!is.na(lost$latfeces[lost$tr=="Control"]), na.rm=T)*100)
latfwshn<-sum(lost$latfeces[lost$tr=="Nutrition + WSH"]==0, na.rm=T)
latfwshperc<-round(latfwshn/sum(!is.na(lost$latfeces[lost$tr=="Nutrition + WSH"]), na.rm=T)*100)
latfeces<-c(latfctrln, latfctrlperc, latfwshn, latfwshperc)

potty<-npercfunc(lost, lost$potty)
feceshouse<-npercfunc(lost, lost$humfeces)
feceschildarea<-npercfunc(lost, lost$humfecesch)
handlatwater<-npercfunc(lost, lost$hwlatwat)
handlatsoap<-npercfunc(lost, lost$hwlatsoap)
handkitwater<-npercfunc(lost, lost$hwkitwat)
handkitsoap<-npercfunc(lost, lost$hwkitsoap)

fsnctrl<-length(lost$hfiacat[lost$tr=="Control" & lost$hfiacat=="Food Secure"])
fspercctrl<-round(fsnctrl/length(lost$hfiacat[lost$tr=="Control"])*100)
fsnwsh<-length(lost$hfiacat[lost$tr=="Nutrition + WSH" & lost$hfiacat=="Food Secure"])
fspercwsh<-round(fsnwsh/length(lost$hfiacat[lost$tr=="Nutrition + WSH"])*100)
foodsecure<-c(fsnctrl, fspercctrl, fsnwsh, fspercwsh)

#make vectors to put in table
#function combines n and percent or mean and sd for vectors created from npercfunc or meansdfunc
#num is 1 if ctrl group, 3 if wsh
charobject<-function(variable, num) {
  paste(variable[num], " (", variable[num+1], ")", sep="")
}

charobjectperc<-function(variable, num) {
  paste(variable[num], " (", variable[num+1], "%)", sep="")
}

ctrl<-c(paste("Control (N=", Nlostctrl, ")", sep=""), " ", charobject(momage, 1),charobject(momeduy, 1), " ", charobject(dadeduy, 1), charobjectperc(dadagri, 1),
        " ", charobject(Nhh, 1), charobjectperc(elec, 1), charobjectperc(cement, 1), charobject(acres, 1),
        " ", charobjectperc(tubewell, 1), charobjectperc(storewater, 1), charobjectperc(treatwater, 1), charobject(waterdis, 1), 
        " ", " ", charobjectperc(odmen, 1), charobjectperc(odwomen, 1), charobjectperc(odchild815, 1), charobjectperc(odchild38, 1), charobjectperc(odchild03, 1), 
        " ", charobjectperc(latowned, 1), charobjectperc(latslab, 1), charobjectperc(latseal, 1), charobjectperc(latfeces, 1),
        charobjectperc(potty, 1), 
        " ", charobjectperc(feceshouse, 1), charobjectperc(feceschildarea, 1), 
        " ", " ", charobjectperc(handlatwater, 1), charobjectperc(handlatsoap, 1), 
        " ", charobjectperc(handkitwater, 1), charobjectperc(handkitsoap, 1), 
        " ", charobjectperc(foodsecure, 1))
wsh<-c(paste("N + WSH Intervention (N=", Nlostwsh, ")", sep=""), " ", charobject(momage, 3),charobject(momeduy, 3), " ", charobject(dadeduy, 3), charobjectperc(dadagri, 3),
       " ", charobject(Nhh, 3), charobjectperc(elec, 3), charobjectperc(cement, 3), charobject(acres, 3),
       " ", charobjectperc(tubewell, 3), charobjectperc(storewater, 3), charobjectperc(treatwater, 3), charobject(waterdis, 3), 
       " ", " ", charobjectperc(odmen, 3), charobjectperc(odwomen, 3), charobjectperc(odchild815, 3), charobjectperc(odchild38, 3), charobjectperc(odchild03, 3), 
       " ", charobjectperc(latowned, 3), charobjectperc(latslab, 3), charobjectperc(latseal, 3), charobjectperc(latfeces, 3),
       charobjectperc(potty, 3), 
       " ", charobjectperc(feceshouse, 3), charobjectperc(feceschildarea, 3), 
       " ", " ", charobjectperc(handlatwater, 3), charobjectperc(handlatsoap, 3), 
       " ", charobjectperc(handkitwater, 3), charobjectperc(handkitsoap, 3), 
       " ", charobjectperc(foodsecure, 3))
 
# Table S1
tbls1<-data.table(" "=c("No. of compounds:", "Maternal", "Age(years)", "Years of education", 
                        "Paternal", "Years of education", "Works in agriculture", 
                        "Household", "Number of people", "Has electricity", "Has a cement floor", "Acres of agricultural land owned", 
                        "Drinking Water", "Shallow tubewell primary water source", "Stored water observed at home", "Reported treating water yesterday", "Distance (mins) to primary water source",
                        "Sanitation", "Reported daily open defecation", "Adult men", "Adult women", "Children: 8 to <15 years", "Children: 3 to <8 years", "Children: 0 to <3 years", 
                        "Latrine", "Owned", "Concrete Slab", "Functional water seal", "Visible stool on slab or floor",
                        "Owned a child potty",
                        "Human feces observed in the", "House", "Child's play area",
                        "Handwashing location", "Within six steps of latrine", "Has water", "Has soap", "Within six steps of kitchen", "Has water", "Has soap", 
                        "Nutrition", "Household is food secure"), 
                  "WASH Benefits Main Trial"=c("Control (N=1382)"," ", "24 (5)", "6 (3)", " ", "5 (4)", "414 (30%)", 
                                               " ", "5 (2)", "784 (57%)", "145 (10%)", "0.15 (0.21)",
                                               " ", "1038 (75%)", "666 (48%)", "4 (0%)", "1 (3)", 
                                               " ", " ", "97 (7%)", "62 (4%)", "53 (10%)", "267 (38%)", "245 (82%)",
                                               " ", "750 (54%)", "1251 (95%)", "358 (31%)", "625 (48%)", "61 (4%)", 
                                               " ", "114 (8%)", "21 (2%)", 
                                               " ", " ", "178 (14%)", "88 (7%)", " ", "118 (9%)", "33 (3%)", " ", "932 (67%)"),
                  " "=c ("N + WSH Intervention (N=686)", " ", "24 (6)", "6 (3)", " ", "5 (4)", "207 (30%)", 
                         " ", "5 (2)", "412 (60%)", "72 (10%)", "0.14 (0.38)",
                         " ", "504 (73%)", "331 (48%)", "2 (0%)", "1 (2)", 
                         " ", " ", "50 (7%)", "24 (4%)", "28 (10%)", "134 (37%)", "123 (88%)",
                         " ", "367 (53%)", "621 (94%)", "155 (27%)", "298 (46%)", "30 (4%)", 
                         " ", "49 (8%)", "7 (1%)", 
                         " ", " ", "72 (11%)", "36 (6%)", " ", "60 (9%)", "18 (3%)", " ", "485 (71%)"), 
                  "Stress Status Study: Had outcomes at Year 1"=data_y1$Children.measured.at.Year.1,
                  " "=data_y1$X..1, 
                  "Stress Status Study: Lost to follow-up at Year 2"=ctrl,
                  " "=wsh
)

write.csv(tbls1, file=here('tables/supplementary/stress_supptable1.csv'))
print(xtable(tbls1), type="html", file=here("tables/supplementary/stress_supptable1.html"))




### Table S2-S3 ####

#to be used for formatting point difference and confidence interval
ci_interval<-function(str, tbl){
  filter<-tbl[tbl$Y == str,]
  if(round(filter[1], 2)==0){ci <- paste(round(filter[1], 3), " (", round(filter[3], 3), ", ", round(filter[4], 3), ")", sep="")
  }else{ci<-paste(round(filter[1], 2), " (", round(filter[3], 2), ", ", round(filter[4], 2), ")", sep="")}
  return(ci)
}

#mean
mean <- function(str, str1, tbl){
  filter <- tbl[tbl$Y == str,]
  filter2 <- filter[filter$tr == str1,]
  paste(round(filter2[3], 2))
}

#sd
sd <- function(str, str1, tbl){
  filter <- tbl[tbl$Y == str,]
  filter2 <- filter[filter$tr == str1,]
  paste(round(filter2[4], 2))
}

#n
n <- function(str, str1, tbl){
  filter <- tbl[tbl$Y == str,]
  filter2 <- filter[filter$tr == str1,]
  paste(round(filter2[5], 2))
}

#pval
pval <- function(str, tbl){
  filter<-tbl[tbl$Y == str,]
  if(filter[5] < 0.01){
    if(filter[5]<0.001){
      return("<0.001")
    }
    return("<0.01")
  }
  paste(round(filter[5], 2))
}

outcomes2<-c("", "iPF(2a)-III (ng/mg creatinine)", "Control", "Nutrition + WSH", "2,3-dinor-iPF(2a)-III (ng/mg creatinine)", 
             "Control", "Nutrition + WSH", "iPF(2a)-VI (ng/mg creatinine)", "Control", "Nutrition + WSH", "8,12-iso-iPF(2a)-VI (ng/mg creatinine)", 
             "Control","Nutrition + WSH")

unadj_diff <-c("95% CI","","", ci_interval("t2_f2_8ip", res_unadj), "","", 
               ci_interval("t2_f2_23d", res_unadj), "","",ci_interval("t2_f2_VI", res_unadj), "","",
               ci_interval("t2_f2_12i", res_unadj))

unadj_pval <-c("P-value","","", pval("t2_f2_8ip", res_unadj), "","", 
               pval("t2_f2_23d", res_unadj), "","",pval("t2_f2_VI", res_unadj), "","",
               pval("t2_f2_12i", res_unadj))

age_sex_adj <- c("95% CI","","", ci_interval("t2_f2_8ip", res_sex),"","", 
                 ci_interval("t2_f2_23d", res_sex), "","",ci_interval("t2_f2_VI", res_sex), "","",
                 ci_interval("t2_f2_12i", res_sex))

age_sex_adj_pval <-c("P-value","","", pval("t2_f2_8ip", res_sex), "","", 
                     pval("t2_f2_23d", res_sex), "","",pval("t2_f2_VI", res_sex), "","",
                     pval("t2_f2_12i", res_sex))

full_adj <- c("95% CI","","", ci_interval("t2_f2_8ip", res_adj),"","", 
              ci_interval("t2_f2_23d", res_adj), "","",ci_interval("t2_f2_VI", res_adj), "","",
              ci_interval("t2_f2_12i", res_adj))

adj_pval <-c("P-value","","", pval("t2_f2_8ip", res_adj), "","", 
             pval("t2_f2_23d", res_adj), "","",pval("t2_f2_VI", res_adj), "","",
             pval("t2_f2_12i", res_adj))

n_t2 <- c("","", n("t2_f2_8ip", "Control", mean_sd_tr), n("t2_f2_8ip", "Nutrition + WSH", mean_sd_tr),"",
          n("t2_f2_23d", "Control", mean_sd_tr), n("t2_f2_23d", "Nutrition + WSH", mean_sd_tr), "",
          n("t2_f2_VI", "Control", mean_sd_tr),n("t2_f2_VI", "Nutrition + WSH", mean_sd_tr), "",
          n("t2_f2_12i", "Control", mean_sd_tr), n("t2_f2_12i", "Nutrition + WSH", mean_sd_tr))

mean_tr <- c("","", mean("t2_f2_8ip", "Control", mean_sd_tr), mean("t2_f2_8ip", "Nutrition + WSH", mean_sd_tr),"",
             mean("t2_f2_23d", "Control", mean_sd_tr), mean("t2_f2_23d", "Nutrition + WSH", mean_sd_tr), "",
             mean("t2_f2_VI", "Control", mean_sd_tr),mean("t2_f2_VI", "Nutrition + WSH", mean_sd_tr), "",
             mean("t2_f2_12i", "Control", mean_sd_tr), mean("t2_f2_12i", "Nutrition + WSH", mean_sd_tr))

sd_t2 <- c("","", sd("t2_f2_8ip", "Control", mean_sd_tr), sd("t2_f2_8ip", "Nutrition + WSH", mean_sd_tr),"",
           sd("t2_f2_23d", "Control", mean_sd_tr), sd("t2_f2_23d", "Nutrition + WSH", mean_sd_tr), "",
           sd("t2_f2_VI", "Control", mean_sd_tr),sd("t2_f2_VI", "Nutrition + WSH", mean_sd_tr), "",
           sd("t2_f2_12i", "Control", mean_sd_tr), sd("t2_f2_12i", "Nutrition + WSH", mean_sd_tr))

abs_mean <- c("","", mean("t2_f2_8ip_raw", "Control", absolute_mean_sd_tr), mean("t2_f2_8ip_raw", "Nutrition + WSH", absolute_mean_sd_tr),"",
              mean("t2_f2_23d_raw", "Control", absolute_mean_sd_tr), mean("t2_f2_23d_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
              mean("t2_f2_VI_raw", "Control", absolute_mean_sd_tr),mean("t2_f2_VI_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
              mean("t2_f2_12i_raw", "Control", absolute_mean_sd_tr), mean("t2_f2_12i_raw", "Nutrition + WSH", absolute_mean_sd_tr))

ipcw_adj <- c("95% CI","","", ci_interval("t2_f2_8ip", res_ipcw),"","", 
              ci_interval("t2_f2_23d", res_ipcw), "","",ci_interval("t2_f2_VI", res_ipcw), "","",
              ci_interval("t2_f2_12i", res_ipcw))

ipcw_pval <-c("P-value","","", pval("t2_f2_8ip", res_ipcw), "","", 
             pval("t2_f2_23d", res_ipcw), "","",pval("t2_f2_VI", res_ipcw), "","",
             pval("t2_f2_12i", res_ipcw))

tbls2 <- data.table(
  "Outcome" = outcomes2,
  "N" = n_t2,
  "Absolute Mean" = abs_mean,
  "Mean" = mean_tr,
  "SD" = sd_t2,
  "Unadjusted difference: Intervention v. Control" = unadj_diff, 
  " " = unadj_pval,
  "Age- and sex-adjusted difference: Intervention v. Control" = age_sex_adj,
  " " = age_sex_adj_pval, 
  "Fully adjusted difference: Intervention v. Control" = full_adj,
  " " = adj_pval,
  "IPCW adjusted difference: Intervention v. Control" = ipcw_adj,
  " " = ipcw_pval
)

outcomes3<-c("", "Pre-stressor Salivary alpha-amylase (U/ml)" ,"Control", "Nutrition + WSH",
             "Post-stressor Salivary alpha-amylase (U/ml)","Control", "Nutrition + WSH",
             "Change in slope between pre- and post-stressor alpha-amylase","Control", "Nutrition + WSH",
             "Residualized gain score for alpha-amylase","Control", "Nutrition + WSH",
             "Pre-stressor salivary cortisol (ug/dl)","Control", "Nutrition + WSH",
             "Post-stressor salivary cortisol (ug/dl)","Control", "Nutrition + WSH",
             "Change in slope between pre- and post-stressor cortisol","Control", "Nutrition + WSH",
             "Residualized gain score for cortisol","Control", "Nutrition + WSH",
             "Mean arterial pressure (mmHg)","Control", "Nutrition + WSH",
             "Resting heart rate (bpm)","Control", "Nutrition + WSH",
             "NR3C1 exon 1F promoter methylation","Control", "Nutrition + WSH",
             "NGFI-A transcription factor binding site","Control", "Nutrition + WSH"
)

unadj_diff3 <-c("95% CI","","", ci_interval("t3_saa_z01", res_unadj), "","", 
                ci_interval("t3_saa_z02", res_unadj), "","",ci_interval("t3_saa_slope", res_unadj), "","",
                ci_interval("t3_residual_saa", res_unadj),
                "","",ci_interval("t3_cort_z01", res_unadj),"","",ci_interval("t3_cort_z03", res_unadj),
                "","",ci_interval("t3_cort_slope", res_unadj),"","",ci_interval("t3_residual_cort", res_unadj),
                "","",ci_interval("t3_map", res_unadj),"","",ci_interval("t3_hr_mean", res_unadj),
                "","",ci_interval("t3_gcr_mean", res_unadj),"","",ci_interval("t3_gcr_cpg12", res_unadj))

unadj_pval3 <-c("P-value","","", pval("t3_saa_z01", res_unadj), "","", 
                pval("t3_saa_z02", res_unadj), "","",pval("t3_saa_slope", res_unadj), "","",
                pval("t3_residual_saa", res_unadj),
                "","",pval("t3_cort_z01", res_unadj),"","",pval("t3_cort_z03", res_unadj),
                "","",pval("t3_cort_slope", res_unadj),"","",pval("t3_residual_cort", res_unadj),
                "","",pval("t3_map", res_unadj),"","",pval("t3_hr_mean", res_unadj),
                "","",pval("t3_gcr_mean", res_unadj),"","",pval("t3_gcr_cpg12", res_unadj))

age_sex_adj3 <- c("95% CI", "","", ci_interval("t3_saa_z01", res_sex), "","", 
                  ci_interval("t3_saa_z02", res_sex), "","",ci_interval("t3_saa_slope", res_sex), "","",
                  ci_interval("t3_residual_saa", res_sex),
                  "","",ci_interval("t3_cort_z01", res_sex),"","",ci_interval("t3_cort_z03", res_sex),
                  "","",ci_interval("t3_cort_slope", res_sex),"","",ci_interval("t3_residual_cort", res_sex),
                  "","",ci_interval("t3_map", res_sex),"","",ci_interval("t3_hr_mean", res_sex),
                  "","",ci_interval("t3_gcr_mean", res_sex),"","",ci_interval("t3_gcr_cpg12", res_sex))

age_sex_pval3 <-c("P-value","","", pval("t3_saa_z01", res_sex), "","", 
                  pval("t3_saa_z02", res_sex), "","",pval("t3_saa_slope", res_sex), "","",
                  pval("t3_residual_saa", res_sex),
                  "","",pval("t3_cort_z01", res_sex),"","",pval("t3_cort_z03", res_sex),
                  "","",pval("t3_cort_slope", res_sex),"","",pval("t3_residual_cort", res_sex),
                  "","",pval("t3_map", res_sex),"","",pval("t3_hr_mean", res_sex),
                  "","",pval("t3_gcr_mean", res_sex),"","",pval("t3_gcr_cpg12", res_sex))

full_adj3 <- c("95% CI","","", ci_interval("t3_saa_z01", res_adj), "","", 
               ci_interval("t3_saa_z02", res_adj), "","",ci_interval("t3_saa_slope", res_adj), "","",
               ci_interval("t3_residual_saa", res_adj),
               "","",ci_interval("t3_cort_z01", res_adj),"","",ci_interval("t3_cort_z03", res_adj),
               "","",ci_interval("t3_cort_slope", res_adj),"","",ci_interval("t3_residual_cort", res_adj),
               "","",ci_interval("t3_map", res_adj),"","",ci_interval("t3_hr_mean", res_adj),
               "","",ci_interval("t3_gcr_mean", res_adj),"","",ci_interval("t3_gcr_cpg12", res_adj))

adj_pval3 <-c("P-value","","", pval("t3_saa_z01", res_adj), "","", 
              pval("t3_saa_z02", res_adj), "","",pval("t3_saa_slope", res_adj), "","",
              pval("t3_residual_saa", res_adj),
              "","",pval("t3_cort_z01", res_adj),"","",pval("t3_cort_z03", res_adj),
              "","",pval("t3_cort_slope", res_adj),"","",pval("t3_residual_cort", res_adj),
              "","",pval("t3_map", res_adj),"","",pval("t3_hr_mean", res_adj),
              "","",pval("t3_gcr_mean", res_adj),"","",pval("t3_gcr_cpg12", res_adj))

mean_tr3 <- c("","", mean("t3_saa_z01", "Control", mean_sd_tr), mean("t3_saa_z01", "Nutrition + WSH", mean_sd_tr),"",
              mean("t3_saa_z02", "Control", mean_sd_tr), mean("t3_saa_z02", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_saa_slope", "Control", mean_sd_tr),mean("t3_saa_slope", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_residual_saa", "Control", mean_sd_tr), mean("t3_residual_saa", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_cort_z01", "Control", mean_sd_tr),mean("t3_cort_z01", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_cort_z03", "Control", mean_sd_tr),mean("t3_cort_z03", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_cort_slope", "Control", mean_sd_tr),mean("t3_cort_slope", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_residual_cort", "Control", mean_sd_tr),mean("t3_residual_cort", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_map", "Control", mean_sd_tr),mean("t3_map", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_hr", "Control", mean_sd_tr),mean("t3_hr", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_gcr", "Control", mean_sd_tr),mean("t3_gcr", "Nutrition + WSH", mean_sd_tr), "",
              mean("t3_gcr_cpg12", "Control", mean_sd_tr),mean("t3_gcr_cpg12", "Nutrition + WSH", mean_sd_tr) )

n_t3 <- c("","", n("t3_saa_z01", "Control", mean_sd_tr), n("t3_saa_z01", "Nutrition + WSH", mean_sd_tr),"",
          n("t3_saa_z02", "Control", mean_sd_tr), n("t3_saa_z02", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_saa_slope", "Control", mean_sd_tr),n("t3_saa_slope", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_residual_saa", "Control", mean_sd_tr), n("t3_residual_saa", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_cort_z01", "Control", mean_sd_tr),n("t3_cort_z01", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_cort_z03", "Control", mean_sd_tr),n("t3_cort_z03", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_cort_slope", "Control", mean_sd_tr),n("t3_cort_slope", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_residual_cort", "Control", mean_sd_tr),n("t3_residual_cort", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_map", "Control", mean_sd_tr),n("t3_map", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_hr", "Control", mean_sd_tr),n("t3_hr", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_gcr", "Control", mean_sd_tr),n("t3_gcr", "Nutrition + WSH", mean_sd_tr), "",
          n("t3_gcr_cpg12", "Control", mean_sd_tr), n("t3_gcr_cpg12", "Nutrition + WSH", mean_sd_tr) )

sd_t3 <- c("","", sd("t3_saa_z01", "Control", mean_sd_tr), sd("t3_saa_z01", "Nutrition + WSH", mean_sd_tr),"",
           sd("t3_saa_z02", "Control", mean_sd_tr), sd("t3_saa_z02", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_saa_slope", "Control", mean_sd_tr),sd("t3_saa_slope", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_residual_saa", "Control", mean_sd_tr), sd("t3_residual_saa", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_cort_z01", "Control", mean_sd_tr),sd("t3_cort_z01", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_cort_z03", "Control", mean_sd_tr),sd("t3_cort_z03", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_cort_slope", "Control", mean_sd_tr),sd("t3_cort_slope", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_residual_cort", "Control", mean_sd_tr),sd("t3_residual_cort", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_map", "Control", mean_sd_tr),sd("t3_map", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_hr", "Control", mean_sd_tr),sd("t3_hr", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_gcr", "Control", mean_sd_tr),sd("t3_gcr", "Nutrition + WSH", mean_sd_tr), "",
           sd("t3_gcr_cpg12", "Control", mean_sd_tr), sd("t3_gcr_cpg12", "Nutrition + WSH", mean_sd_tr) )

abs_mean_t3 <- c("","", mean("t3_saa_z01_raw", "Control", absolute_mean_sd_tr), mean("t3_saa_z01_raw", "Nutrition + WSH", absolute_mean_sd_tr),"",
                 mean("t3_saa_z02_raw", "Control", absolute_mean_sd_tr), mean("t3_saa_z02_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_saa_slope", "Control", absolute_mean_sd_tr),mean("t3_saa_slope", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_residual_saa", "Control", absolute_mean_sd_tr), mean("t3_residual_saa", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_cort_z01_raw", "Control", absolute_mean_sd_tr),mean("t3_cort_z01_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_cort_z03_raw", "Control", absolute_mean_sd_tr),mean("t3_cort_z03_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_cort_slope", "Control", absolute_mean_sd_tr),mean("t3_cort_slope", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_residual_cort", "Control", absolute_mean_sd_tr),mean("t3_residual_cort", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_map", "Control", absolute_mean_sd_tr),mean("t3_map", "Nutrition + WSH", mean_sd_tr), "",
                 mean("t3_hr", "Control", absolute_mean_sd_tr),mean("t3_hr", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_gcr_raw", "Control", absolute_mean_sd_tr),mean("t3_gcr_raw", "Nutrition + WSH", absolute_mean_sd_tr), "",
                 mean("t3_gcr_cpg12_raw", "Control", absolute_mean_sd_tr),mean("t3_gcr_cpg12_raw", "Nutrition + WSH", absolute_mean_sd_tr) )

ipcw_adj3 <-c("95% CI","","", ci_interval("t3_saa_z01", res_ipcw), "","", 
                ci_interval("t3_saa_z02", res_ipcw), "","",ci_interval("t3_saa_slope", res_ipcw), "","",
                ci_interval("t3_residual_saa", res_ipcw),
                "","",ci_interval("t3_cort_z01", res_ipcw),"","",ci_interval("t3_cort_z03", res_ipcw),
                "","",ci_interval("t3_cort_slope", res_ipcw),"","",ci_interval("t3_residual_cort", res_ipcw),
                "","",ci_interval("t3_map", res_ipcw),"","",ci_interval("t3_hr_mean", res_ipcw),
                "","",ci_interval("t3_gcr_mean", res_ipcw),"","",ci_interval("t3_gcr_cpg12", res_ipcw))

ipcw_pval3 <-c("P-value","","", pval("t3_saa_z01", res_ipcw), "","", 
              pval("t3_saa_z02", res_ipcw), "","",pval("t3_saa_slope", res_ipcw), "","",
              pval("t3_residual_saa", res_ipcw),
              "","",pval("t3_cort_z01", res_ipcw),"","",pval("t3_cort_z03", res_ipcw),
              "","",pval("t3_cort_slope", res_ipcw),"","",pval("t3_residual_cort", res_ipcw),
              "","",pval("t3_map", res_ipcw),"","",pval("t3_hr_mean", res_ipcw),
              "","",pval("t3_gcr_mean", res_ipcw),"","",pval("t3_gcr_cpg12", res_ipcw))

tbls3 <- data.table(
  "Outcome" = outcomes3,
  "N" = n_t3,
  "Absolute Mean" = abs_mean_t3,
  "Mean" = mean_tr3,
  "SD" = sd_t3,
  "Unadjusted difference: Intervention v. Control" = unadj_diff3, 
  " " = unadj_pval3,
  "Age- and sex-adjusted difference: Intervention v. Control" = age_sex_adj3,
  " " = age_sex_pval3, 
  "Fully adjusted difference: Intervention v. Control" = full_adj3,
  " " = adj_pval3,
  "IPCW adjusted difference: Intervention v. Control" = ipcw_adj3,
  " " = ipcw_pval3
)

write.csv(tbls2, here('tables/supplementary/miso9-stress-supplementarytable2.csv'))
write.csv(tbls3, here('tables/supplementary/miso9-stress-supplementarytable3.csv'))

####Table S 7: subgroup analyses by sex at t2#####
pval <- function(num){
  if(num < 0.01){
    if(num<0.001){
      return("<0.001")
    }
    return("<0.01")
  }
  as.character(round(num, 2))
}
#Outcomes

outcomes7 <- c( "Biomarker", "iPF(2α)-III (ng/mg creatinine)", "2,3-dinor-iPF(2α±)-III (ng/mg creatinine)", "iPF(2α±)-VI (ng/mg creatinine)", "8,12-iso-iPF(2α±)-VI (ng/mg creatinine)")

#N
n7.0 <- c("n", as.character(round(absolute_mean_sd_sex$n[1], 2)), as.character(round(absolute_mean_sd_sex$n[3], 2)), as.character(round(absolute_mean_sd_sex$n[5], 2)), as.character(round(absolute_mean_sd_sex$n[7], 2)))

n7.1 <- c("n", as.character(round(absolute_mean_sd_sex$n[2], 2)), as.character(round(absolute_mean_sd_sex$n[4], 2)), as.character(round(absolute_mean_sd_sex$n[6], 2)), as.character(round(absolute_mean_sd_sex$n[8], 2)))


#mean
mean7.0 <- c("Mean", as.character(round(absolute_mean_sd_sex$mean[1], 2)), as.character(round(absolute_mean_sd_sex$mean[3], 2)), as.character(round(absolute_mean_sd_sex$mean[5], 2)), as.character(round(absolute_mean_sd_sex$mean[7], 2)))

mean7.1 <- c("Mean", as.character(round(absolute_mean_sd_sex$mean[2], 2)), as.character(round(absolute_mean_sd_sex$mean[4], 2)), as.character(round(absolute_mean_sd_sex$mean[6], 2)), as.character(round(absolute_mean_sd_sex$mean[8], 2)))

#sd

sd7.0 <- c("SD", as.character(round(absolute_mean_sd_sex$sd[1], 2)), as.character(round(absolute_mean_sd_sex$sd[3], 2)), as.character(round(absolute_mean_sd_sex$sd[5], 2)), as.character(round(absolute_mean_sd_sex$sd[7], 2)))

sd7.1 <- c("SD", as.character(round(absolute_mean_sd_sex$sd[2], 2)), as.character(round(absolute_mean_sd_sex$sd[4], 2)), as.character(round(absolute_mean_sd_sex$sd[6], 2)), as.character(round(absolute_mean_sd_sex$sd[8], 2)))

#Difference measures - Female (0)

#RD (mean difference) 

rd7.0 <- c(as.character(round(res_sub$RD[1], 2)), as.character(round(res_sub$RD[3], 2)), as.character(round(res_sub$RD[5], 2)), as.character(round(res_sub$RD[7], 2)))

#lb

lb7.0 <- c(as.character(round(res_sub$ci.l[1], 2)), as.character(round(res_sub$ci.l[3], 2)), as.character(round(res_sub$ci.l[5], 2)), as.character(round(res_sub$ci.l[7], 2)))

#ub

ub7.0 <- c(as.character(round(res_sub$ci.u[1], 2)), as.character(round(res_sub$ci.u[3], 2)), as.character(round(res_sub$ci.u[5], 2)), as.character(round(res_sub$ci.u[7], 2)))

#P value
pval7.0 <- c("P-value", pval(res_sub$Pval[1]), pval(res_sub$Pval[3]), pval(res_sub$Pval[5]), pval(res_sub$Pval[7]))

intp7 <- c(" ", pval(res_sub$intP[1]), pval(res_sub$intP[3]), pval(res_sub$intP[5]), pval(res_sub$intP[7]))

#Combine RD and CI vectors
RD_CI_7.0 <- paste0(rd7.0, "(", lb7.0, ",", ub7.0,")")
RD_CI_7.0 <- c("Unadjusted difference: Intervention vs. Control (95% CI)", RD_CI_7.0)

#Difference measures - Male (1)

#RD (mean difference) 

rd7.1 <- c(as.character(round(res_sub$RD[2], 2)), as.character(round(res_sub$RD[4], 2)), as.character(round(res_sub$RD[6], 2)), as.character(round(res_sub$RD[8], 2)))

#lb

lb7.1 <- c(as.character(round(res_sub$ci.l[2], 2)), as.character(round(res_sub$ci.l[4], 2)), as.character(round(res_sub$ci.l[6], 2)), as.character(round(res_sub$ci.l[8], 2)))

#ub

ub7.1 <- c(as.character(round(res_sub$ci.u[2], 2)), as.character(round(res_sub$ci.u[4], 2)), as.character(round(res_sub$ci.u[6], 2)), as.character(round(res_sub$ci.u[8], 2)))

#P value
pval7.1 <- c("P-value", pval(res_sub$Pval[2]), pval(res_sub$Pval[4]), pval(res_sub$Pval[6]), pval(res_sub$Pval[8]))

#Combine RD and CI vectors
RD_CI_7.1 <- paste0(rd7.1, "(", lb7.1, ",", ub7.1,")")
RD_CI_7.1 <- c("Unadjusted difference: Intervention vs. Control (95% CI)", RD_CI_7.1)


#Combine RD and CI vectors
RD_CI_7 <- paste0(rd7, " (", lb7, ",", ub7,")")
RD_CI_7 <- c("Unadjusted difference: Male vs. Female (95% CI)", RD_CI_7)
#Question - is the CI for res_sex the main effect of sex?

#create table 7
tbls7 <- data.table( 
  " " = outcomes7,
  "Female" =  n7.0, 
  " " = mean7.0,
  " " = sd7.0,
  " " = RD_CI_7.0,
  " " = pval7.0,
  "Male" =  n7.1, 
  " " = mean7.1,
  " " = sd7.1,
  " " = RD_CI_7.1,
  " " = pval7.1,
  "Interaction P-value" = intp7
  # " " = RD_CI_7,
  # " " = pval7
)


# unable to write code to this location - I think I need to create a new folder in Git
write.csv(tbls7, file=here('tables/supplementary/miso9-stress-supplementarytable4.csv'))
print(xtable(tbls7), type="html", file=here('tables/supplementary/miso9-stress-supplementarytable4.html'))

###Table S 8: subgroup analysis by sex at t3#######

#outcomes


outcomes8 <- c("Stress biomarker", "Mean arterial pressure (mmHg)", "Resting heart rate (bpm)", "Pre-stressor alpha-amylase (U/ml)", "Post-stressor alpha-amylase (U/ml)", "Pre-stressor cortisol (ug/dl)", "Post-stressor cortisol (ug/dl)", "NR3C1 exon 1F promoter methylation", "NGFI-A transcription factor binding site", "Change in slope between pre- and post-stressor alpha-amylase", "Change in slope between pre- and post-stressor cortisol", "Residualized gain score for alpha-amylase", "Residualized gain score for cortisol")


#N
n8.0 <- c("n", as.character(round(absolute_mean_sd_sex$n[9], 2)), as.character(round(absolute_mean_sd_sex$n[11], 2)), as.character(round(absolute_mean_sd_sex$n[13], 2)), as.character(round(absolute_mean_sd_sex$n[15], 2)), as.character(round(absolute_mean_sd_sex$n[17], 2)), as.character(round(absolute_mean_sd_sex$n[19], 2)), as.character(round(absolute_mean_sd_sex$n[21], 2)), as.character(round(absolute_mean_sd_sex$n[23], 2)), as.character(round(absolute_mean_sd_sex$n[25], 2)), as.character(round(absolute_mean_sd_sex$n[27], 2)), as.character(round(absolute_mean_sd_sex$n[29], 2)), as.character(round(absolute_mean_sd_sex$n[31], 2)))

n8.1 <- c("n", as.character(round(absolute_mean_sd_sex$n[10], 2)), as.character(round(absolute_mean_sd_sex$n[12], 2)), as.character(round(absolute_mean_sd_sex$n[14], 2)), as.character(round(absolute_mean_sd_sex$n[16], 2)), as.character(round(absolute_mean_sd_sex$n[18], 2)), as.character(round(absolute_mean_sd_sex$n[20], 2)), as.character(round(absolute_mean_sd_sex$n[22], 2)), as.character(round(absolute_mean_sd_sex$n[24], 2)), as.character(round(absolute_mean_sd_sex$n[26], 2)), as.character(round(absolute_mean_sd_sex$n[28], 2)), as.character(round(absolute_mean_sd_sex$n[30], 2)), as.character(round(absolute_mean_sd_sex$n[32], 2)))


#mean
mean8.0 <- c("Mean", as.character(round(absolute_mean_sd_sex$mean[9], 2)), as.character(round(absolute_mean_sd_sex$mean[11], 2)), as.character(round(absolute_mean_sd_sex$mean[13], 2)), as.character(round(absolute_mean_sd_sex$mean[15], 2)), as.character(round(absolute_mean_sd_sex$mean[17], 2)), as.character(round(absolute_mean_sd_sex$mean[19], 2)), as.character(round(absolute_mean_sd_sex$mean[21], 2)), as.character(round(absolute_mean_sd_sex$mean[23], 2)), as.character(round(absolute_mean_sd_sex$mean[25], 2)), as.character(round(absolute_mean_sd_sex$mean[27], 2)), as.character(round(absolute_mean_sd_sex$mean[29], 2)), as.character(round(absolute_mean_sd_sex$mean[31], 2)))

mean8.1 <- c("Mean", as.character(round(absolute_mean_sd_sex$mean[10], 2)), as.character(round(absolute_mean_sd_sex$mean[12], 2)), as.character(round(absolute_mean_sd_sex$mean[14], 2)), as.character(round(absolute_mean_sd_sex$mean[16], 2)), as.character(round(absolute_mean_sd_sex$mean[18], 2)), as.character(round(absolute_mean_sd_sex$mean[20], 2)), as.character(round(absolute_mean_sd_sex$mean[22], 2)), as.character(round(absolute_mean_sd_sex$mean[24], 2)), as.character(round(absolute_mean_sd_sex$mean[26], 2)), as.character(round(absolute_mean_sd_sex$mean[28], 2)), as.character(round(absolute_mean_sd_sex$mean[30], 2)), as.character(round(absolute_mean_sd_sex$mean[32], 2)))

#sd

sd8.0 <- c("SD", as.character(round(absolute_mean_sd_sex$sd[9], 2)), as.character(round(absolute_mean_sd_sex$sd[11], 2)), as.character(round(absolute_mean_sd_sex$sd[13], 2)), as.character(round(absolute_mean_sd_sex$sd[15], 2)), as.character(round(absolute_mean_sd_sex$sd[17], 2)), as.character(round(absolute_mean_sd_sex$sd[19], 2)), as.character(round(absolute_mean_sd_sex$sd[21], 2)), as.character(round(absolute_mean_sd_sex$sd[23], 2)), as.character(round(absolute_mean_sd_sex$sd[25], 2)), as.character(round(absolute_mean_sd_sex$sd[27], 2)), as.character(round(absolute_mean_sd_sex$sd[29], 2)), as.character(round(absolute_mean_sd_sex$sd[31], 2)))

sd8.1 <- c("SD", as.character(round(absolute_mean_sd_sex$sd[10], 2)), as.character(round(absolute_mean_sd_sex$sd[12], 2)), as.character(round(absolute_mean_sd_sex$sd[14], 2)), as.character(round(absolute_mean_sd_sex$sd[16], 2)), as.character(round(absolute_mean_sd_sex$sd[18], 2)), as.character(round(absolute_mean_sd_sex$sd[20], 2)), as.character(round(absolute_mean_sd_sex$sd[22], 2)), as.character(round(absolute_mean_sd_sex$sd[24], 2)), as.character(round(absolute_mean_sd_sex$sd[26], 2)), as.character(round(absolute_mean_sd_sex$sd[28], 2)), as.character(round(absolute_mean_sd_sex$sd[30], 2)), as.character(round(absolute_mean_sd_sex$sd[32], 2)))

#Difference measures - Female (0)

#RD (mean difference) 

rd8.0 <- c(as.character(round(res_sub$RD[9], 2)), as.character(round(res_sub$RD[11], 2)), as.character(round(res_sub$RD[13], 2)), as.character(round(res_sub$RD[15], 2)), as.character(round(res_sub$RD[17], 2)), as.character(round(res_sub$RD[19], 2)), as.character(round(res_sub$RD[21], 2)), as.character(round(res_sub$RD[23], 2)), as.character(round(res_sub$RD[25], 2)), as.character(round(res_sub$RD[27], 2)), as.character(round(res_sub$RD[29], 2)), as.character(round(res_sub$RD[31], 2)))

rd8.1 <- c(as.character(round(res_sub$RD[10], 2)), as.character(round(res_sub$RD[12], 2)), as.character(round(res_sub$RD[14], 2)), as.character(round(res_sub$RD[16], 2)), as.character(round(res_sub$RD[18], 2)), as.character(round(res_sub$RD[20], 2)), as.character(round(res_sub$RD[22], 2)), as.character(round(res_sub$RD[24], 2)), as.character(round(res_sub$RD[26], 2)), as.character(round(res_sub$RD[28], 2)), as.character(round(res_sub$RD[30], 2)), as.character(round(res_sub$RD[32], 2)))

#lb

lb8.0 <- c(as.character(round(res_sub$ci.l[9], 2)), as.character(round(res_sub$ci.l[11], 2)), as.character(round(res_sub$ci.l[13], 2)), as.character(round(res_sub$ci.l[15], 2)), as.character(round(res_sub$ci.l[17], 2)), as.character(round(res_sub$ci.l[19], 2)), as.character(round(res_sub$ci.l[21], 2)), as.character(round(res_sub$ci.l[23], 2)), as.character(round(res_sub$ci.l[25], 2)), as.character(round(res_sub$ci.l[27], 2)), as.character(round(res_sub$ci.l[29], 2)), as.character(round(res_sub$ci.l[31], 2)))

lb8.1 <- c(as.character(round(res_sub$ci.l[10], 2)), as.character(round(res_sub$ci.l[12], 2)), as.character(round(res_sub$ci.l[14], 2)), as.character(round(res_sub$ci.l[16], 2)), as.character(round(res_sub$ci.l[18], 2)), as.character(round(res_sub$ci.l[20], 2)), as.character(round(res_sub$ci.l[22], 2)), as.character(round(res_sub$ci.l[24], 2)), as.character(round(res_sub$ci.l[26], 2)), as.character(round(res_sub$ci.l[28], 2)), as.character(round(res_sub$ci.l[30], 2)), as.character(round(res_sub$ci.l[32], 2)))

#ub

ub8.0 <- c(as.character(round(res_sub$ci.u[9], 2)), as.character(round(res_sub$ci.u[11], 2)), as.character(round(res_sub$ci.u[13], 2)), as.character(round(res_sub$ci.u[15], 2)), as.character(round(res_sub$ci.u[17], 2)), as.character(round(res_sub$ci.u[19], 2)), as.character(round(res_sub$ci.u[21], 2)), as.character(round(res_sub$ci.u[23], 2)), as.character(round(res_sub$ci.u[25], 2)), as.character(round(res_sub$ci.u[27], 2)), as.character(round(res_sub$ci.u[29], 2)), as.character(round(res_sub$ci.u[31], 2)))

ub8.1 <- c(as.character(round(res_sub$ci.u[10], 2)), as.character(round(res_sub$ci.u[12], 2)), as.character(round(res_sub$ci.u[14], 2)), as.character(round(res_sub$ci.u[16], 2)), as.character(round(res_sub$ci.u[18], 2)), as.character(round(res_sub$ci.u[20], 2)), as.character(round(res_sub$ci.u[22], 2)), as.character(round(res_sub$ci.u[24], 2)), as.character(round(res_sub$ci.u[26], 2)), as.character(round(res_sub$ci.u[28], 2)), as.character(round(res_sub$ci.u[30], 2)), as.character(round(res_sub$ci.u[32], 2)))

#P value
pval7.0 <- c("P-value", as.character(round(res_sub$Pval[1], 2)), as.character(round(res_sub$Pval[3], 2)), as.character(round(res_sub$Pval[5], 2)), as.character(round(res_sub$Pval[7], 2)))

pval8.0 <- c("P-value", pval(res_sub$Pval[9]), pval(res_sub$Pval[11]), pval(res_sub$Pval[13]), pval(res_sub$Pval[15]), pval(res_sub$Pval[17]), pval(res_sub$Pval[19]), pval(res_sub$Pval[21]), pval(res_sub$Pval[23]), pval(res_sub$Pval[25]), pval(res_sub$Pval[27]), pval(res_sub$Pval[29]), pval(res_sub$Pval[31]))

pval8.1 <- c("P-value", pval(res_sub$Pval[10]), pval(res_sub$Pval[12]), pval(res_sub$Pval[14]), pval(res_sub$Pval[16]), pval(res_sub$Pval[18]), pval(res_sub$Pval[20]), pval(res_sub$Pval[22]), pval(res_sub$Pval[24]), pval(res_sub$Pval[26]), pval(res_sub$Pval[28]), pval(res_sub$Pval[30]), pval(res_sub$Pval[32]))

intpval8 <-  c("P-value", pval(res_sub$intP[10]), pval(res_sub$intP[12]), pval(res_sub$intP[14]), pval(res_sub$intP[16]), pval(res_sub$intP[18]), pval(res_sub$intP[20]), pval(res_sub$intP[22]), pval(res_sub$intP[24]), pval(res_sub$intP[26]), pval(res_sub$intP[28]), pval(res_sub$intP[30]), pval(res_sub$intP[32]))

#Combine RD and CI vectors
RD_CI_8.0 <- paste0(rd8.0, " (", lb8.0, ",", ub8.0,")")
RD_CI_8.0 <- c("Unadjusted difference: Intervention vs. Control (95% CI)", RD_CI_8.0)

RD_CI_8.1 <- paste0(rd8.0, " (", lb8.1, ",", ub8.1,")")
RD_CI_8.1 <- c("Unadjusted difference: Intervention vs. Control (95% CI)", RD_CI_8.1)

#create table 8
tbls8 <- data.table( 
  " " = outcomes8,
  "Female" =  n8.0, 
  " " = mean8.0,
  " " = sd8.0,
  " " = RD_CI_8.0,
  " " = pval8.0,
  "Male" =  n8.1, 
  " " = mean8.1,
  " " = sd8.1,
  " " = RD_CI_8.1,
  " " = pval8.1,
  "Interaction P-value" = intpval8
  # " " = RD_CI_7,
  # " " = pval7
)

write.csv(tbls8, file=here('tables/supplementary/miso9-stress-supplementarytable5.csv'))
print(xtable(tbls8), type="html", file=here('tables/supplementary/miso9-stress-supplementarytable5.html'))


