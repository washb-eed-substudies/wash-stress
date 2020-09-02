rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(ggpubr)


# Load results
load(here::here("results/stress_results.Rdata"))

# Function for helping make table with name and age attached
readjustfunc <- function(data, var){
  data[data$Y == var,]
}



#### FIGURE UNADJUSTED DIFFERENCES ####


# group the "F2 isoprostanes" together as you've done already, 
#"Hypothalamic-pituitary-adrenal axis": cortisol measures + NR3C1, and NGFI-A
#third group would be "Sympathetic adrenomedullary axis": which includes 
#salivary alpha amylase/heart rate/mean arterial pressure.  
#Aesthetically, it might make sense to include units in the legend.

d <- rbind(
  data.frame(readjustfunc(res_unadj, "t2_f2_8ip"), name="iPF(2a)-III (ng/mg creatinine)", age=14, group="F2 Isoprostanes"),
  data.frame(readjustfunc(res_unadj, "t2_f2_23d"), name="2,3-dinor-iPF(2a)-III (ng/mg creatinine)", age=14, group="F2 Isoprostanes"),
  data.frame(readjustfunc(res_unadj, "t2_f2_VI"), name="iPF(2a)-VI (ng/mg creatinine)", age=14, group="F2 Isoprostanes"),
  data.frame(readjustfunc(res_unadj, "t2_f2_12i"), name="8,12-iso-iPF(2a)-VI (ng/mg creatinine)", age=14, group="F2 Isoprostanes"),
  data.frame(readjustfunc(res_unadj, "t3_saa_z01"), name="Pre-stressor salivary alpha-amylase (U/ml)", age=28, group="Sympathetic adrenomedullary axis"),
  data.frame(readjustfunc(res_unadj, "t3_saa_z02"), name="Post-stressor salivary alpha-amylase (U/ml)", age=28, group="Sympathetic adrenomedullary axis"),
  data.frame(readjustfunc(res_unadj, "t3_saa_slope"), name="Change in slope between pre- and \n post-stressor alpha-amylase)", age=28, group="Sympathetic adrenomedullary axis"),
  data.frame(readjustfunc(res_unadj, "t3_residual_saa"), name="Residualized gain score for alpha-amylase", age=28, group="Salimetrics\ngain scores"),
  data.frame(readjustfunc(res_unadj, "t3_cort_z01"), name="Pre-stressor salivary cortisol (ug/dl)", age=28, group="Hypothalamic-pituitary-adrenal axis"),
  data.frame(readjustfunc(res_unadj, "t3_cort_z03"), name="Post-stressor salivary cortisol (ug/dl)", age=28, group="Hypothalamic-pituitary-adrenal axis"),
  data.frame(readjustfunc(res_unadj, "t3_cort_slope"), name="Change in slope between pre- and \n post-stressor cortisol", age=28, group="Hypothalamic-pituitary-adrenal axis"),
  data.frame(readjustfunc(res_unadj, "t3_residual_cort"), name="Residualized gain score for cortisol", age=28, group="Salimetrics\ngain scores"),
  data.frame(readjustfunc(res_unadj, "t3_map"), name="Mean arterial pressure (mmHg)", age=28, group="Sympathetic adrenomedullary axis"),
  data.frame(readjustfunc(res_unadj, "t3_hr_mean"), name="Resting heart rate (bpm)", age=28, group="Sympathetic adrenomedullary axis"),
  data.frame(readjustfunc(res_unadj, "t3_gcr_mean"), name="NR3C1 exon 1F promoter methylation", age=28, group="Hypothalamic-pituitary-adrenal axis"),
  data.frame(readjustfunc(res_unadj, "t3_gcr_cpg12"), name="NGFI-A transcription factor binding site", age=28, group="Hypothalamic-pituitary-adrenal axis")
) %>% arrange(group)

d$age <- as.factor(d$age)
d$group <- factor(d$group, levels=unique(d$group))
d$name <- factor(d$name, levels=unique(d$name))

d$tr <- c("Control v. Nutrition + Water + Sanitation + Handwashing")


#drop salimetrics
plotdf <- d %>% filter(group!="Salimetrics\ngain scores")

p <- ggplot(plotdf, (aes(x=name, y=Mean.difference))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=ci.l, ymax=ci.u),
                width = 0.3, size = 1) +
  geom_hline(yintercept = 0) +
  facet_wrap(~group, ncol=1, scales="free_y") +
  coord_flip() +
  labs(y = "Mean difference", x = "Biomarker") +
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(0, "lines")) 
p


ggsave(p, file = here::here("figures/stress_forest_diff.png"), height=9, width=8)


#### FIGURE TREATMENT SPECIFIC MEANS ####

mean_ci_tr <- mean_ci_tr %>% 
                rename(
                  Y=outcome,
                  ci.l=Lower.95.CI,
                  ci.u=Upper.95.CI,
                  Treatment=tr
                ) %>%
              mutate(Treatment=factor(Treatment, levels=c("Nutrition + WSH", "Control")))

d <- rbind(
  data.frame(readjustfunc(mean_ci_tr, "t2_f2_8ip"), name="iPF(2a)-III (ng/mg creatinine)", age=14, group="F2 Isoprostanes"),
  data.frame(readjustfunc(mean_ci_tr, "t2_f2_23d"), name="2,3-dinor-iPF(2a)-III (ng/mg creatinine)", age=14, group="F2 Isoprostanes"),
  data.frame(readjustfunc(mean_ci_tr, "t2_f2_VI"), name="iPF(2a)-VI (ng/mg creatinine)", age=14, group="F2 Isoprostanes"),
  data.frame(readjustfunc(mean_ci_tr, "t2_f2_12i"), name="8,12-iso-iPF(2a)-VI (ng/mg creatinine)", age=14, group="F2 Isoprostanes"),
  data.frame(readjustfunc(mean_ci_tr, "t3_saa_z01"), name="Pre-stressor salivary alpha-amylase (U/ml)", age=28, group="Salimetrics"),
  data.frame(readjustfunc(mean_ci_tr, "t3_saa_z02"), name="Post-stressor salivary alpha-amylase (U/ml)", age=28, group="Salimetrics"),
  data.frame(readjustfunc(mean_ci_tr, "t3_saa_slope"), name="Change in slope between pre- and \n post-stressor alpha-amylase", age=28, group="Salimetrics"),
  data.frame(readjustfunc(mean_ci_tr, "t3_residual_saa"), name="Residualized gain score for alpha-amylase", age=28, group="Salimetrics\ngain scores"),
  data.frame(readjustfunc(mean_ci_tr, "t3_cort_z01"), name="Pre-stressor salivary cortisol (ug/dl)", age=28, group="Salimetrics"),
  data.frame(readjustfunc(mean_ci_tr, "t3_cort_z03"), name="Post-stressor salivary cortisol (ug/dl)", age=28, group="Salimetrics"),
  data.frame(readjustfunc(mean_ci_tr, "t3_cort_slope"), name="Change in slope between pre- and \n post-stressor cortisol", age=28, group="Salimetrics"),
  data.frame(readjustfunc(mean_ci_tr, "t3_residual_cort"), name="Residualized gain score for cortisol", age=28, group="Salimetrics\ngain scores"),
  data.frame(readjustfunc(mean_ci_tr, "t3_map"), name="Mean arterial pressure (mmHg)", age=28, group="Vitals"),
  data.frame(readjustfunc(mean_ci_tr, "t3_hr_mean"), name="Resting heart rate (bpm)", age=28, group="Vitals"),
  data.frame(readjustfunc(mean_ci_tr, "t3_gcr_mean"), name="NR3C1 exon 1F promoter methylation", age=28, group="Vitals"),
  data.frame(readjustfunc(mean_ci_tr, "t3_gcr_cpg12"), name="NGFI-A transcription factor binding site", age=28, group="Vitals")
)

d$age <- as.factor(d$age)
plotdf <- d %>% filter(group!="Salimetrics\ngain scores")

p <- ggplot(plotdf, (aes(x=name, y=Mean, group=Treatment, color=Treatment, fill=Treatment))) + 
  geom_point(size=3, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin=ci.l, ymax=ci.u),
                width = 0.3, size = 1, position = position_dodge(0.5)) +
  facet_wrap(~group, ncol=1, scales="free") +
  coord_flip() +
  scale_color_manual(values = tableau10[c(2,1)]) +
  scale_fill_manual(values = tableau10[c(2,1)]) +
  labs(y = "Log-transformed Mean", x = "Biomarker") +
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(0, "lines"))  +
  guides(colour=guide_legend(reverse=TRUE), fill=guide_legend(reverse=TRUE))

p

ggsave(p, file = here::here("figures/stress_forest_mean.png"), height=9, width=8)

