
source(here::here("0-config.R"))
d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS"))
colnames(d)
summary(d$t3_cort_slope)
d$t3_cort_z01




# Plot
library(ggplot2)
p1 <- ggplot(d, aes(x=t3_cort_z01, xend=t3_cort_z01, y=t3_cort_z03, yend=t3_cort_z03, color=tr)) +
  geom_point(aes(x=t3_cort_z01, y=t3_cort_z01), alpha=0.5) +  # Initial points
  geom_point(aes(x=t3_cort_z01, y=t3_cort_z03),  alpha=0.5) +  # End points
  geom_segment(aes(y=t3_cort_z01), #arrow=arrow(type="closed", length=unit(0.1,"inches")),
               alpha=0.5) +
  labs(title="Trajectories from t3_cort_z01 to Follow-Up by Intervention Arm",
       x="t3_cort_z01 Measurement", y="Follow-Up Measurement") +
  theme_minimal()

p_facet <- p1 + facet_wrap(~tr)


d_before <- d %>% select(childid, t3_cort_z01, tr) %>% rename(cort=t3_cort_z01) %>% mutate(time="Pre-stressor")
d_after <- d %>% select(childid, t3_cort_z03, tr) %>% rename(cort=t3_cort_z03) %>% mutate(time="Post-stressor")
plotdf <- bind_rows(d_before, d_after) %>% mutate(time=factor(time, levels=c("Pre-stressor","Post-stressor"))) %>% 
  filter(!cort%>%is.na())
ptraj <- ggplot(plotdf, aes(x=time, y=cort, color=tr, group=childid )) + 
  geom_point(size=2, alpha=0.5, position = position_dodge(0.1)) + 
  geom_line(size=1.5, alpha=0.05, position = position_dodge(0.1)) + 
  labs(x="Measurement time", y="Ln Salivary cortisol (\u03bcg/dl)")

ptraj +  facet_wrap(~tr)

plotdf %>% mutate(id=match(childid, unique(childid))) %>% select(!childid) %>% write_csv("/Users/sophiatan/Documents/WASH/wash-stress-source-suppfig2.csv")
#proportion of non-responders


# Q1. To address the reviewer's comment in yellow, what's the threshold for non-responders? 55.95% of measurements are within a standard 
#deviation (SD) of the change, while 8.25% are between 0.1 and -0.21. Or is a non-responder anyone with 0 or a negative response?

#   Doug: We define a meaningful change between two cortisol time point measures as 2x the lower limit of sensitivity of the assay and 
#2x the average coefficient of variation between duplicate tests of the same sample. The logic is that to be a difference between two 
#scores that we care about that difference needs to be larger than the inherent error in the measure itself. We have used this 
#criterion.... Absolute difference between pre and post of at least +.02 ug/dL (2x the lower limit of sensitivity which is .01 ug/dL) 
#and a change of at least +10% (2x the average CV of 5%). Both these have to be true for us to identify an individual as a "reactor - 
#a case that shows a reliable/measurable increase in cortisol".  
#You could decide to apply this to define "Increase", "no-change", 
#and "Decrease"... "Decrease" would be < - .02 ug/dl change and < -10%. 
#No-change would be the group in between these two thresholds...
#in this case the difference was not large enough in either direection for us to be able to call an increase or decrease- the difference 
#was not larger than the inherent error in the assay.

# The lower limit of sensitivity for the salivary cortisol assay is 0.007 ??g/dL.  
# The coefficient of variation for the assay is 10%

#note: need to use non-z-scored
d$t3_cort_z01_raw

summary(d$t3_cort_z03 - d$t3_cort_z01)
d$cort_difference = d$t3_cort_z03 - d$t3_cort_z01
prop.table(table(abs(d$cort_difference) < sd(d$cort_difference, na.rm=T)))
prop.table(table(abs(d$cort_difference) < 0.1))

d <- d %>% mutate(cort_difference = t3_cort_z03_raw - t3_cort_z01_raw,
                  perc_change = ((t3_cort_z03_raw - t3_cort_z01_raw)/t3_cort_z01_raw) *100,
                  responder_cat=case_when(cort_difference > 0.007 *2 & perc_change > 20 ~"Reactor",
                                      cort_difference < -0.007 *2 & perc_change < -20  ~"Non-reactor",
                                      (abs(cort_difference) <= (0.007 *2) & !is.na(cort_difference)) | 
                                        (abs(perc_change) < 20 & !is.na(perc_change))  ~"No-change"))
head(d)

(0.060-0.055)/0.055 * 100

table(abs(d$cort_difference)>0.007*2)
table(abs(d$perc_change)>20)

table(!is.na(d$cort_difference))
summary(d$cort_difference)
summary(d$perc_change)
table(!is.na(d$responder_cat))
table(d$responder_cat) 

d %>% select(t3_cort_z01_raw, t3_cort_z03_raw, cort_difference, perc_change, responder_cat)

#need to Z-score the 



# plotting 3 lines (increase, no-change, decrease) or labelling perhaps as "reactors", "no-change", and "non-reactors"...  
#that could be simple...


#make both stratified and not stratified by intervention arm
d_before <- d %>% select(childid, t3_cort_z01_raw, tr, responder_cat) %>% rename(cort=t3_cort_z01_raw) %>% mutate(time="Pre-stressor")
d_after <- d %>% select(childid, t3_cort_z03_raw, tr, responder_cat) %>% rename(cort=t3_cort_z03_raw) %>% mutate(time="Post-stressor")
plotdf <- bind_rows(d_before, d_after)
ptraj <- ggplot(plotdf, aes(x=time, y=cort, color=responder_cat, group=responder_cat )) + 
  geom_point(size=2, alpha=0.5, position = position_dodge(0.1)) + 
  geom_line(size=1.5, alpha=0.05, position = position_dodge(0.1)) + 
  labs(x="Measurement time", y="Salivary cortisol (\u03bcg/dl)")
ptraj

#why are the lines not connecting!?

ptraj_tr <- ptraj +  facet_wrap(~tr)
ptraj_tr
