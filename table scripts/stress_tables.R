rm(list=ls())
library("xtable")
source(here::here("0-config.R"))

load(here("results/stress_results.RData"))
d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS")) %>% distinct(childid, .keep_all = TRUE)

#### TABLE 1 ####

filtering <- function(row){
  any(!is.na(row))
}

y1<-d[apply(select(d, grep("t2_f2", names(d), ignore.case=T)), 1, filtering),]

y2<-d[apply(select(d, c("t3_map","t3_hr_mean","t3_saa_z01","t3_saa_z02","t3_cort_z01","t3_cort_z03",
                  "t3_gcr_mean","t3_gcr_cpg12","t3_saa_slope","t3_cort_slope","t3_residual_saa","t3_residual_cort")), 1, filtering),]


#calculating overall N by arm
y1Nctrl<-length(y1$tr[y1$tr=="Control"])
y1Nwsh<-length(y1$tr[y1$tr=="Nutrition + WSH"])
y2Nctrl<-length(y2$tr[y2$tr=="Control"])
y2Nwsh<-length(y2$tr[y2$tr=="Nutrition + WSH"])

#functions for calculating %/mean for all variables in table based on arm
meansdfunc <- function(tbl, variable) {
  ctrlmean<-round(mean(variable[tbl$tr=="Control"], na.rm=TRUE),1)
  ctrlsd<-round(sd(variable[tbl$tr=="Control"], na.rm=TRUE),1)
  wshmean<-round(mean(variable[tbl$tr=="Nutrition + WSH"], na.rm=TRUE),1)
  wshsd<-round(sd(variable[tbl$tr=="Nutrition + WSH"], na.rm=TRUE),1)
  c(ctrlmean, ctrlsd, wshmean, wshsd)
}

npercfunc <- function(tbl, variable) {
  ctrln<-sum(variable[tbl$tr=="Control"], na.rm=TRUE)
  ctrlperc<-round(mean(variable[tbl$tr=="Control"], na.rm=TRUE)*100)
  wshn<-sum(variable[tbl$tr=="Nutrition + WSH"], na.rm=TRUE)
  wshperc<-round(mean(variable[tbl$tr=="Nutrition + WSH"], na.rm=TRUE)*100)
  c(ctrln, ctrlperc, wshn, wshperc)
}

y1momage<-meansdfunc(y1, y1$momage)
y1momeduy<-meansdfunc(y1, y1$momeduy)
y1dadeduy<-meansdfunc(y1, y1$dadeduy)
y1dadagri<-npercfunc(y1, y1$dadagri)
y1Nhh<-meansdfunc(y1, y1$Nhh)
y1elec<-npercfunc(y1, y1$elec)
y1cement<-npercfunc(y1, y1$cement)

y2momage<-meansdfunc(y2, y2$momage)
y2momeduy<-meansdfunc(y2, y2$momeduy)
y2dadeduy<-meansdfunc(y2, y2$dadeduy)
y2dadagri<-npercfunc(y2, y2$dadagri)
y2Nhh<-meansdfunc(y2, y2$Nhh)
y2elec<-npercfunc(y2, y2$elec)
y2cement<-npercfunc(y2, y2$cement)

y1acresctrlm<-round(mean(y1$landacre[y1$tr=="Control"], na.rm=TRUE), 1)
y1acresctrlsd<-round(sd(y1$landacre[y1$tr=="Control"], na.rm=TRUE), 1)
y1acreswshm<-round(mean(y1$landacre[y1$tr=="Nutrition + WSH"], na.rm=TRUE), 1)
y1acreswshsd<-round(mean(y1$landacre[y1$tr=="Nutrition + WSH"], na.rm=TRUE), 1)
y1acres<-c(y1acresctrlm, y1acresctrlsd, y1acreswshm, y1acreswshsd)

y2acresctrlm<-round(mean(y2$landacre[y2$tr=="Control"], na.rm=TRUE), 1)
y2acresctrlsd<-round(sd(y2$landacre[y2$tr=="Control"], na.rm=TRUE), 1)
y2acreswshm<-round(mean(y2$landacre[y2$tr=="Nutrition + WSH"], na.rm=TRUE), 1)
y2acreswshsd<-round(mean(y2$landacre[y2$tr=="Nutrition + WSH"], na.rm=TRUE), 1)
y2acres<-c(y2acresctrlm, y2acresctrlsd, y2acreswshm, y2acreswshsd)

y1tubewell<-npercfunc(y1, y1$tubewell)
y1storewater<-npercfunc(y1, y1$storewat)
y1treatwater<-npercfunc(y1, y1$treatwat)
y1waterdis<-meansdfunc(y1, y1$watmin)
y1odmen<-npercfunc(y1, y1$odmen)
y1odwomen<-npercfunc(y1, y1$odwom)
y1odchild815<-npercfunc(y1, y1$odch815)
y1odchild38<-npercfunc(y1, y1$odch38)
y1odchild03<-npercfunc(y1, y1$odchu3)
y1latowned<-npercfunc(y1, y1$latown)
y1latslab<-npercfunc(y1, y1$latslab)
y1latseal<-npercfunc(y1, y1$latseal)

y1latfctrln<-sum(y1$latfeces[y1$tr=="Control"]==0, na.rm=T)
y1latfctrlperc<-round(y1latfctrln/sum(!is.na(y1$latfeces[y1$tr=="Control"]), na.rm=T)*100)
y1latfwshn<-sum(y1$latfeces[y1$tr=="Nutrition + WSH"]==0, na.rm=T)
y1latfwshperc<-round(y1latfwshn/sum(!is.na(y1$latfeces[y1$tr=="Nutrition + WSH"]), na.rm=T)*100)
y1latfeces<-c(y1latfctrln, y1latfctrlperc, y1latfwshn, y1latfwshperc)

y1potty<-npercfunc(y1, y1$potty)
y1feceshouse<-npercfunc(y1, y1$humfeces)
y1feceschildarea<-npercfunc(y1, y1$humfecesch)
y1handlatwater<-npercfunc(y1, y1$hwlatwat)
y1handlatsoap<-npercfunc(y1, y1$hwlatsoap)
y1handkitwater<-npercfunc(y1, y1$hwkitwat)
y1handkitsoap<-npercfunc(y1, y1$hwkitsoap)

y2tubewell<-npercfunc(y2, y2$tubewell)
y2storewater<-npercfunc(y2, y2$storewat)
y2treatwater<-npercfunc(y2, y2$treatwat)
y2waterdis<-meansdfunc(y2, y2$watmin)
y2odmen<-npercfunc(y2, y2$odmen)
y2odwomen<-npercfunc(y2, y2$odwom)
y2odchild815<-npercfunc(y2, y2$odch815)
y2odchild38<-npercfunc(y2, y2$odch38)
y2odchild03<-npercfunc(y2, y2$odchu3)
y2latowned<-npercfunc(y2, y2$latown)
y2latslab<-npercfunc(y2, y2$latslab)
y2latseal<-npercfunc(y2, y2$latseal)

y2latfctrln<-sum(y2$latfeces[y2$tr=="Control"]==0, na.rm=T)
y2latfctrlperc<-round(y2latfctrln/sum(!is.na(y2$latfeces[y2$tr=="Control"]), na.rm=T)*100)
y2latfwshn<-sum(y2$latfeces[y2$tr=="Nutrition + WSH"]==0, na.rm=T)
y2latfwshperc<-round(y2latfwshn/sum(!is.na(y2$latfeces[y2$tr=="Nutrition + WSH"]), na.rm=T)*100)
y2latfeces<-c(y2latfctrln, y2latfctrlperc, y2latfwshn, y2latfwshperc)

y2potty<-npercfunc(y2, y2$potty)
y2feceshouse<-npercfunc(y2, y2$humfeces)
y2feceschildarea<-npercfunc(y2, y2$humfecesch)
y2handlatwater<-npercfunc(y2, y2$hwlatwat)
y2handlatsoap<-npercfunc(y2, y2$hwlatsoap)
y2handkitwater<-npercfunc(y2, y2$hwkitwat)
y2handkitsoap<-npercfunc(y2, y2$hwkitsoap)

y1fsctrln<-length(y1$hfiacat[y1$tr=="Control" & y1$hfiacat=="Food Secure"])
y1fsctrlperc<-round(y1fsctrln/length(y1$hfiacat[y1$tr=="Control"])*100)
y1fswshn<-length(y1$hfiacat[y1$tr=="Nutrition + WSH" & y1$hfiacat=="Food Secure"])
y1fswshperc<-round(y1fswshn/length(y1$hfiacat[y1$tr=="Nutrition + WSH"])*100)
y1foodsecure<-c(y1fsctrln, y1fsctrlperc, y1fswshn, y1fswshperc)

y2fsctrln<-length(y2$hfiacat[y2$tr=="Control" & y2$hfiacat=="Food Secure"])
y2fsctrlperc<-round(y2fsctrln/length(y2$hfiacat[y2$tr=="Control"])*100)
y2fswshn<-length(y2$hfiacat[y2$tr=="Nutrition + WSH" & y2$hfiacat=="Food Secure"])
y2fswshperc<-round(y2fswshn/length(y2$hfiacat[y2$tr=="Nutrition + WSH"])*100)
y2foodsecure<-c(y2fsctrln, y2fsctrlperc, y2fswshn, y2fswshperc)

#make vectors to put in table
#function combines n and percent or mean and sd for vectors created from npercfunc or meansdfunc
#num is 1 if ctrl group, 3 if wsh
charobject<-function(variable, num) {
  paste(variable[num], " (", variable[num+1], ")", sep="")
}

charobjectperc<-function(variable, num) {
  paste(variable[num], " (", variable[num+1], "%)", sep="")
}

ctrly1<-c(paste("Control (N=", y1Nctrl, ")", sep=""), " ", charobject(y1momage, 1),charobject(y1momeduy, 1), " ", charobject(y1dadeduy, 1), charobjectperc(y1dadagri, 1),
          " ", charobject(y1Nhh, 1), charobjectperc(y1elec, 1), charobjectperc(y1cement, 1), charobject(y1acres, 1),
          " ", charobjectperc(y1tubewell, 1), charobjectperc(y1storewater, 1), charobjectperc(y1treatwater, 1), charobject(y1waterdis, 1), 
          " ", " ", charobjectperc(y1odmen, 1), charobjectperc(y1odwomen, 1), charobjectperc(y1odchild815, 1), charobjectperc(y1odchild38, 1), charobjectperc(y1odchild03, 1), 
          " ", charobjectperc(y1latowned, 1), charobjectperc(y1latslab, 1), charobjectperc(y1latseal, 1), charobjectperc(y1latfeces, 1),
          charobjectperc(y1potty, 1), 
          " ", charobjectperc(y1feceshouse, 1), charobjectperc(y1feceschildarea, 1), 
          " ", " ", charobjectperc(y1handlatwater, 1), charobjectperc(y1handlatsoap, 1), 
          " ", charobjectperc(y1handkitwater, 1), charobjectperc(y1handkitsoap, 1), 
          " ", charobjectperc(y1foodsecure, 1))

wshy1<-c(paste("N+WSH Intervention (N=", y1Nwsh, ")", sep=""), " ", charobject(y1momage, 3),charobject(y1momeduy, 3), " ", charobject(y1dadeduy, 3), charobjectperc(y1dadagri, 3),
         " ", charobject(y1Nhh, 3), charobjectperc(y1elec, 3), charobjectperc(y1cement, 3), charobject(y1acres, 3),
         " ", charobjectperc(y1tubewell, 3), charobjectperc(y1storewater, 3), charobjectperc(y1treatwater, 3), charobject(y1waterdis, 3), 
         " ", " ", charobjectperc(y1odmen, 3), charobjectperc(y1odwomen, 3), charobjectperc(y1odchild815, 3), charobjectperc(y1odchild38, 3), charobjectperc(y1odchild03, 3), 
         " ", charobjectperc(y1latowned, 3), charobjectperc(y1latslab, 3), charobjectperc(y1latseal, 3), charobjectperc(y1latfeces, 3),
         charobjectperc(y1potty, 3), 
         " ", charobjectperc(y1feceshouse, 3), charobjectperc(y1feceschildarea, 3), 
         " ", " ", charobjectperc(y1handlatwater, 3), charobjectperc(y1handlatsoap, 3), 
         " ", charobjectperc(y1handkitwater, 3), charobjectperc(y1handkitsoap, 3), 
         " ", charobjectperc(y1foodsecure, 3))

ctrly2<-c(paste("Control (N=", y2Nctrl, ")", sep=""), " ", charobject(y2momage, 1),charobject(y2momeduy, 1), " ", charobject(y2dadeduy, 1), charobjectperc(y2dadagri, 1),
          " ", charobject(y2Nhh, 1), charobjectperc(y2elec, 1), charobjectperc(y2cement, 1), charobject(y2acres, 1),
          " ", charobjectperc(y2tubewell, 1), charobjectperc(y2storewater, 1), charobjectperc(y2treatwater, 1), charobject(y2waterdis, 1), 
          " ", " ", charobjectperc(y2odmen, 1), charobjectperc(y2odwomen, 1), charobjectperc(y2odchild815, 1), charobjectperc(y2odchild38, 1), charobjectperc(y2odchild03, 1), 
          " ", charobjectperc(y2latowned, 1), charobjectperc(y2latslab, 1), charobjectperc(y2latseal, 1), charobjectperc(y2latfeces, 1),
          charobjectperc(y2potty, 1), 
          " ", charobjectperc(y2feceshouse, 1), charobjectperc(y2feceschildarea, 1), 
          " ", " ", charobjectperc(y2handlatwater, 1), charobjectperc(y2handlatsoap, 1), 
          " ", charobjectperc(y2handkitwater, 1), charobjectperc(y2handkitsoap, 1), 
          " ", charobjectperc(y2foodsecure, 1))

wshy2<-c(paste("N+WSH Intervention (N=", y2Nwsh, ")", sep=""), " ", charobject(y2momage, 3),charobject(y2momeduy, 3), " ", charobject(y2dadeduy, 3), charobjectperc(y2dadagri, 3),
         " ", charobject(y2Nhh, 3), charobjectperc(y2elec, 3), charobjectperc(y2cement, 3), charobject(y2acres, 3),
         " ", charobjectperc(y2tubewell, 3), charobjectperc(y2storewater, 3), charobjectperc(y2treatwater, 3), charobject(y2waterdis, 3), 
         " ", " ", charobjectperc(y2odmen, 3), charobjectperc(y2odwomen, 3), charobjectperc(y2odchild815, 3), charobjectperc(y2odchild38, 3), charobjectperc(y2odchild03, 3), 
         " ", charobjectperc(y2latowned, 3), charobjectperc(y2latslab, 3), charobjectperc(y2latseal, 3), charobjectperc(y2latfeces, 3),
         charobjectperc(y2potty, 3), 
         " ", charobjectperc(y2feceshouse, 3), charobjectperc(y2feceschildarea, 3), 
         " ", " ", charobjectperc(y2handlatwater, 3), charobjectperc(y2handlatsoap, 3), 
         " ", charobjectperc(y2handkitwater, 3), charobjectperc(y2handkitsoap, 3), 
         " ", charobjectperc(y2foodsecure, 3))

# Table 1: Enrollment characteristics by intervention group
tbl1 <- data.table(
  " " = c("No. of compounds:", "Maternal", "Age(years)", "Years of education", 
          "Paternal", "Years of education", "Works in agriculture", 
          "Household", "Number of people", "Has electricity", "Has a cement floor", "Acres of agricultural land owned", 
          "Drinking Water", "Shallow tubewell primary water source", "Stored water observed at home", "Reported treating water yesterday", "Distance (mins) to primary water source",
          "Sanitation", "Reported daily open defecation", "Adult men", "Adult women", "Children: 8 to <15 years", "Children: 3 to <8 years", "Children: 0 to <3 years", 
          "Latrine", "Owned", "Concrete Slab", "Functional water seal", "Visible stool on slab or floor",
          "Owned a child potty",
          "Human feces observed in the", "House", "Child's play area",
          "Handwashing location", "Within six steps of latrine", "Has water", "Has soap", "Within six steps of kitchen", "Has water", "Has soap", 
          "Nutrition", "Household is food secure"),
  "Children measured at Year 1" = ctrly1,
  " " = wshy1,
  "Children measured at Year 2" = ctrly2,
  " " = wshy2
)

write.csv(tbl1, file=here('tables/main/stress_table1.csv'))
print(xtable(tbl1), type="html", file=here("tables/main/stress_table1.html"))


#### TABLE 2/3 ####

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
  #if(filter[5] < 0.01){
    if(filter[5]<0.001){
      return("<0.001")
    }
    #return("<0.01")
  #}
  paste(format(round(filter[5], 3), nsmall = 3))
}

outcomes2<-c("", "Ln iPF(2a)-III (ng/mg creatinine)", "Control", "Nutrition + WSH", "Ln 2,3-dinor-iPF(2a)-III (ng/mg creatinine)", 
              "Control", "Nutrition + WSH", "Ln iPF(2a)-VI (ng/mg creatinine)", "Control", "Nutrition + WSH", "Ln 8,12-iso-iPF(2a)-VI (ng/mg creatinine)", 
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

full_adj <- c("95% CI", "","", ci_interval("t2_f2_8ip", res_adj),"","", 
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
  " " = adj_pval
)

outcomes3<-c("",
             "Ln Pre-stressor Salivary alpha-amylase (U/ml)" ,"Control", "Nutrition + WSH",
             "Ln Post-stressor Salivary alpha-amylase (U/ml)","Control", "Nutrition + WSH",
             "Slope between pre- and post-stressor alpha-amylase (U/ml/min)","Control", "Nutrition + WSH",
             "Residualized gain score for alpha-amylase","Control", "Nutrition + WSH",
             "Ln Pre-stressor salivary cortisol (ug/dl)","Control", "Nutrition + WSH",
             "Ln Post-stressor salivary cortisol (ug/dl)","Control", "Nutrition + WSH",
             "Slope between pre- and post-stressor cortisol (ug/dl/min)","Control", "Nutrition + WSH",
             "Residualized gain score for cortisol","Control", "Nutrition + WSH",
             "Mean arterial pressure (mmHg)","Control", "Nutrition + WSH",
             "Resting heart rate (bpm)","Control", "Nutrition + WSH",
             "Logit NR3C1 exon 1F promoter methylation","Control", "Nutrition + WSH",
             "Logit NGFI-A transcription factor binding site","Control", "Nutrition + WSH"
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

age_sex_adj3 <- c("95% CI","","", ci_interval("t3_saa_z01", res_sex), "","", 
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
  "Full adjusted difference: Intervention v. Control" = full_adj3,
  " " = adj_pval3
)

write.csv(tbls2, file=here('tables/main/miso9-stress-table2.csv'), row.names = F)
write.csv(tbls3, file=here('tables/main/miso9-stress-table3.csv'), row.names = F)





