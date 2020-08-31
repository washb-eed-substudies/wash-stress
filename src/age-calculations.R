rm(list=ls())
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS"))

#calculate median age and IQR for children with Year 1 and Year 2 outcomes
#function for filtering for only participants with at least one outcome
filtering <- function(row){
  any(!is.na(row))}

y1_has_outcomes<-d[apply(select(d, grep("t2_f2", names(d), ignore.case=T)), 1, filtering),]
y1_quantile_age<-round(quantile(y1_has_outcomes$ageyr_ut2*12, na.rm=T), 2)

y2_has_outcomes<-d[apply(select(d, t3_saa_slope, t3_residual_saa, t3_cort_slope, t3_residual_cort, t3_map, t3_hr_mean, t3_gcr_mean, t3_gcr_cpg12), 1, filtering),]
y2_quantile_age<-round(quantile(d$agemth_t3_vital, na.rm=T), 2)

age<-data.table(" "=c("Age Year 1", "Age Year 2"),
           "median (IQR)"=c(paste(y1_quantile_age[3], " (", y1_quantile_age[2], ", ", y1_quantile_age[4], ")", sep=""),
                            paste(y2_quantile_age[3], " (", y2_quantile_age[2], ", ", y2_quantile_age[4], ")", sep="")))

write.csv(age, file=here('results/age-calculations.csv'))
