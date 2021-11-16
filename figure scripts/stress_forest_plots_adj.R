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
  data.frame(readjustfunc(res_adj, "t2_f2_8ip"), name="iPF(2a)-III (ng/mg creatinine)", age=14, group="Oxidative stress (Year 1)"),
  data.frame(readjustfunc(res_adj, "t2_f2_23d"), name="2,3-dinor-iPF(2a)-III (ng/mg creatinine)", age=14, group="Oxidative stress (Year 1)"),
  data.frame(readjustfunc(res_adj, "t2_f2_VI"), name="iPF(2a)-VI (ng/mg creatinine)", age=14, group="Oxidative stress (Year 1)"),
  data.frame(readjustfunc(res_adj, "t2_f2_12i"), name="8,12-iso-iPF(2a)-VI (ng/mg creatinine)", age=14, group="Oxidative stress (Year 1)"),
  data.frame(readjustfunc(res_adj, "t3_saa_z01"), name="Pre-stressor salivary alpha-amylase (U/ml)", age=28, group="Sympathetic adrenomedullary axis (Year 2)"),
  data.frame(readjustfunc(res_adj, "t3_saa_z02"), name="Post-stressor salivary alpha-amylase (U/ml)", age=28, group="Sympathetic adrenomedullary axis (Year 2)"),
  data.frame(readjustfunc(res_adj, "t3_saa_slope"), name="Slope between pre- and \n post-stressor alpha-amylase (U/ml/min)", age=28, group="Sympathetic adrenomedullary axis (Year 2)"),
  data.frame(readjustfunc(res_adj, "t3_residual_saa"), name="Residualized gain score\nfor alpha-amylase (U/ml)", age=28, group="Sympathetic adrenomedullary axis (Year 2)"),
  data.frame(readjustfunc(res_adj, "t3_cort_z01"), name="Pre-stressor salivary cortisol (\u03bcg/dl)", age=28, group="Hypothalamic-pituitary-adrenal axis (Year 2)"),
  data.frame(readjustfunc(res_adj, "t3_cort_z03"), name="Post-stressor salivary cortisol (\u03bcg/dl)", age=28, group="Hypothalamic-pituitary-adrenal axis (Year 2)"),
  data.frame(readjustfunc(res_adj, "t3_cort_slope"), name="Slope between pre- and \n post-stressor cortisol (\u03bcg/dl/min)", age=28, group="Hypothalamic-pituitary-adrenal axis (Year 2)"),
  data.frame(readjustfunc(res_adj, "t3_residual_cort"), name="Residualized gain score\nfor cortisol (\u03bcg/dl)", age=28, group="Hypothalamic-pituitary-adrenal axis (Year 2)"),
  data.frame(readjustfunc(res_adj, "t3_map"), name="Mean arterial pressure (mmHg)", age=28, group="Sympathetic adrenomedullary axis (Year 2)"),
  data.frame(readjustfunc(res_adj, "t3_hr_mean"), name="Resting heart rate (bpm)", age=28, group="Sympathetic adrenomedullary axis (Year 2)"),
  data.frame(readjustfunc(res_adj, "t3_gcr_mean"), name="Logit-transformed NR3C1 exon\n1F promoter methylation", age=28, group="Hypothalamic-pituitary-adrenal axis (Year 2)"),
  data.frame(readjustfunc(res_adj, "t3_gcr_cpg12"), name="Logit-transformed NGFI-A transcription\nfactor binding site methylation", age=28, group="Hypothalamic-pituitary-adrenal axis (Year 2)")
) %>% 
  mutate(group=factor(group, levels=c("Oxidative stress (Year 1)", "Hypothalamic-pituitary-adrenal axis (Year 2)", "Sympathetic adrenomedullary axis (Year 2)"))) %>%
  arrange(group)


d$age <- as.factor(d$age)
d$group <- factor(d$group, levels=unique(d$group))
d$name <- factor(d$name, levels=rev(unique(d$name)))

d$tr <- c("Control v. Nutrition + Water + Sanitation + Handwashing")


#drop slope
#plotdf <- d %>% filter(group!="Salimetrics\ngain scores")
plotdf <- d %>% filter(!grepl("Slope", name))

p <- ggplot(plotdf, (aes(x=name, y=Mean.difference))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=ci.l, ymax=ci.u),
                width = 0.75, size = 1) +
  geom_hline(yintercept = 0) +
  facet_wrap(~group, ncol=1, scales="free") +
  coord_flip() +
  labs(y = "Mean difference", x = "Biomarker") +
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(0, "lines")) 
p


ggsave(p, file = here::here("figures/stress_forest_diff_adj.png"), height=9, width=8)
