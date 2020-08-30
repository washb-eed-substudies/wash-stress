rm(list=ls())
library("xtable")
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS"))

#calculate median age and IQR for children with Year 1 and Year 2 outcomes
#function for filtering for only participants with at least one outcome
filtering <- function(row){
  any(!is.na(row))}

y1_has_outcomes<-d[apply(select(d, grep("t2_f2", names(d), ignore.case=T)), 1, filtering),]
y1_quantile_age<-quantile(y1_has_outcomes$ageyr_ut2*12, na.rm=T)

y2_has_outcomes<-d[apply(select(d, t3_saa_slope, t3_residual_saa, t3_cort_slope, t3_residual_cort, t3_map, t3_hr_mean, t3_gcr_mean, t3_gcr_cpg12), 1, filtering),]
y2_median_age<-median(y2_has_outcomes$ageyr_ut2*12, na.rm=T)


