
source(here::here("0-config.R"))
library(tibble)
library(dplyr)

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS"))

#function for filtering for only participants with at least one outcome
filtering <- function(row){
  any(!is.na(row))}

y1_has_outcomes<-d[apply(select(d, grep("t2_f2", names(d), ignore.case=T)), 1, filtering),]
y1_ctrl<-y1_has_outcomes[y1_has_outcomes$tr=="Control",]
y1_nwsh<-y1_has_outcomes[y1_has_outcomes$tr=="Nutrition + WSH",]
y1_ctrl_clusters<-length(unique(y1_ctrl$clusterid))
y1_nwsh_clusters<-length(unique(y1_nwsh$clusterid))
y1_ctrl_n<-nrow(distinct(y1_ctrl))
y1_nwsh_n<-nrow(distinct(y1_nwsh))

#y2_has_outcomes<-d[apply(select(d, t3_saa_slope, t3_residual_saa, t3_cort_slope, t3_residual_cort, t3_map, t3_hr_mean, t3_gcr_mean, t3_gcr_cpg12), 1, filtering),]
y2_has_outcomes<-d[apply(select(d, t3_hr_mean), 1, filtering),]
y2_ctrl<-y2_has_outcomes[y2_has_outcomes$tr=="Control",]
y2_nwsh<-y2_has_outcomes[y2_has_outcomes$tr=="Nutrition + WSH",]
y2_ctrl_clusters<-length(unique(y2_ctrl$clusterid))
y2_nwsh_clusters<-length(unique(y2_nwsh$clusterid))
y2_ctrl_n<-nrow(distinct(y2_ctrl))
y2_nwsh_n<-nrow(distinct(y2_nwsh))
length(unique(y2_ctrl$clusterid))

dim(d %>% filter(tr=="Control") %>% distinct(childid, t3_saa_slope, t3_residual_saa, t3_cort_slope, t3_residual_cort, t3_map, t3_hr_mean, t3_gcr_mean, t3_gcr_cpg12))
temp <- d %>% filter(tr=="Control") %>% distinct(childid, t3_saa_slope, t3_residual_saa, t3_cort_slope, t3_residual_cort, t3_map, t3_hr_mean, t3_gcr_mean, t3_gcr_cpg12)
temp <- temp[apply(select(temp, tr, t3_saa_slope, t3_residual_saa, t3_cort_slope, t3_residual_cort, t3_map, t3_hr_mean, t3_gcr_mean, t3_gcr_cpg12), 1, filtering),]
length(unique(temp$childid))
write.csv(temp, file='C:/Users/andre/Downloads/washb_stress_y2_control.csv')

library(readxl)

enrol_mid <- read_excel("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/Enrollment Records/EE Midline Enrollment Record v6_27Jan2017_FINAL_AL_corrected.xlsx", sheet = 'Individual Sample Record')
colnames(enrol_mid)[1] <- 'clusterid'
enrol_mid$clusterid <- as.numeric(enrol_mid$clusterid)
tr <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/washb-bangladesh-tr (real).csv")

enrol_mid <- left_join(enrol_mid, tr, by="clusterid") %>% filter(tr=='Control'|tr=='Nutrition + WSH')
table(enrol_mid$tr)
table(enrol_mid$`If No Consent, Give Reason`)
table(enrol_mid$tr,enrol_mid$`If No Consent, Give Reason`)



enrol <- read_excel("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/Enrollment Records/EE Endline Enrollment Record_27Jan2017_AL_corrected.xlsx", sheet = 'Individual Sample Record')
colnames(enrol)[1] <- 'clusterid'
enrol$clusterid <- as.numeric(enrol$clusterid)
tr <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/washb-bangladesh-tr (real).csv")

enrol <- left_join(enrol, tr, by="clusterid") %>% filter(tr=='Control'|tr=='Nutrition + WSH')
table(enrol$tr)
table(enrol$`If No Consent, Give Reason`)
table(enrol$tr,enrol$`If No Consent, Give Reason`)

enrol <- enrol %>% filter(`Collection Form on File? 1=Yes / 0=No`==1)
enrol$childid <- as.numeric(enrol$`Child Barcode ID (Cluster ID + HH ID)`) * 10 + 1
enrol2 <- enrol %>% filter(childid %in% temp$childid)

table(floor(temp$childid/10) %in% as.numeric(enrol$`Child Barcode ID (Cluster ID + HH ID)`))

table(enrol2$`Consent Given?                   1=Yes / 0=No`)
table(enrol2$`Child Oximeter Collected?    1=Yes / 0=No / P=Partial`)
table(enrol2$`Child BP Collected?    1=Yes / 0=No / P=Partial`)
table(enrol2$`Plasma Blood Tube Collected?            1=Yes / 0=No /P=Partial`)
table(enrol2$`Serum Blood Tube Collected?            1=Yes / 0=No /P=Partial`)
table(enrol2$`Any blood`)
table(enrol2$`Oragene Saliva Collected?                            1=Yes / 0=No / P=Partial`)
table(enrol2$`Stool Collected?                            1=Yes / 0=No / P=Partial`)
table(enrol2$`Pre-LM Urine Collected?                 1=Yes / 0=No / P=Partial`)



library(SmartEDA)
ExpReport(enrol2,op_file='smartEDA.html')

library(DataExplorer)
create_report(enrol2)
