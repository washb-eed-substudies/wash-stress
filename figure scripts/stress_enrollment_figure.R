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
y1_ctrl_n<-nrow(y1_ctrl)
y1_nwsh_n<-nrow(y1_nwsh)

y2_has_outcomes<-d[apply(select(d, t3_saa_slope, t3_residual_saa, t3_cort_slope, t3_residual_cort, t3_map, t3_hr_mean, t3_gcr_mean, t3_gcr_cpg12), 1, filtering),]
y2_ctrl<-y2_has_outcomes[y2_has_outcomes$tr=="Control",]
y2_nwsh<-y2_has_outcomes[y2_has_outcomes$tr=="Nutrition + WSH",]
y2_ctrl_clusters<-length(unique(y2_ctrl$clusterid))
y2_nwsh_clusters<-length(unique(y2_nwsh$clusterid))
y2_ctrl_n<-nrow(y2_ctrl)
y2_nwsh_n<-nrow(y2_nwsh)


data <- tibble(x = 1:100, y= 1:100)
head(data)
data %>% 
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
  theme_void() ->
  p

p +
  geom_rect(xmin = 25, xmax=75, ymin=96, ymax=100, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=98,label= '13,279 compounds assessed for eligibility', size=2.5) ->#+
  #annotate('text', x= 50, y=102,label= 'Figure S1: CONSORT Diagram for the WASH Benefits immune status and growth factor study population', size=3) ->
  p

p +
  geom_rect(xmin = 58, xmax=104, ymin=87, ymax=95, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 81, y=90,label= 'Excluded: 7,728 compounds \n 7,429 compounds excluded to create bu???er zones\n 219 compounds did not meet enrollment criteria\n 80 compounds declined to participate
', size=2.5) +
  annotate('text', x= 3, y=90,label= 'Enrollment', size=4) +
  geom_rect(xmin = 30, xmax=70, ymin=82, ymax=86, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=84.6,label= '
720 clusters created and randomly allocated across 7 arms \n 5,551 compounds randomly allocated across 7 arms \n 2 out of 7 arms selected into substudy', size=2.5)  +
  annotate('text', x= 2.5, y=84,label= 'Allocation', size=4) +
  geom_rect(xmin = 9, xmax=25, ymin=76, ymax=82, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 17, y=80,label= '
Control \n 180 clusters \n 1,382 households', size=2.5) +
  geom_rect(xmin = 71, xmax=104, ymin=76, ymax=82, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 88, y=80,label= '
Water+Sanitation+Handwashing+Nutrition \n 90 clusters \n 686 households ', size=2.5) +
  geom_rect(xmin = 71, xmax=104, ymin=63, ymax=75, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 88, y=70,label= '
Year 1 \n 63 clusters \n 480 children \n Year 2 \n 67 clusters \n 505 children ', size=2.5)+
  geom_rect(xmin = 71, xmax=104, ymin=32, ymax=62, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 88, y=48,label= '
Year 1 \n 100 children lost to follow-up \n 9 moved \n 29 absent \n 14 withdrew \n 37 no live birth \n 11 child death \n Year 2 \n 25 new children measured  \n 104 children lost to follow-up \n 28 moved \n 2 absent \n 18 withdrew \n 38 no live birth \n 18 child death ', size=2.5) +
  geom_rect(xmin = 71, xmax=104, ymin=19, ymax=31, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 88, y=26,label= '
Year 1 \n 63 clusters \n 380 children \n Year 2 \n 67 clusters \n 401 children ', size=2.5) + 
  geom_rect(xmin = 71, xmax=104, ymin=10, ymax=18, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 88, y=15,label= paste(
"Year 1 \n ", 380-y1_nwsh_n, " missing outcome \n Year 2 \n ", 401-y2_nwsh_n, " missing outcome", sep=""), size=2.5) + 
  geom_rect(xmin = 71, xmax=104, ymin=-3, ymax=9, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 88, y=4,label= paste(
"Year 1 \n ", y1_nwsh_clusters, " clusters \n ", y1_nwsh_n, " children \n Year 2 \n ", y2_nwsh_clusters, " clusters \n ", y2_nwsh_n, " children", sep=""), size=2.5) +
  geom_rect(xmin = 9, xmax=25, ymin=63, ymax=75, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 17, y=70,label= '
Year 1 \n 68 clusters \n 516 children \n Year 2 \n 68 clusters \n 516 children ', size=2.5) +
  annotate('text', x= 3, y=70,label= 'Subsample \n Target', size=3.5) +
  geom_rect(xmin = 6, xmax=28, ymin=32, ymax=62, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 17, y=48,label= '
Year 1 \n 140 children lost to follow-up \n 14 moved \n 16 absent \n 62 withdrew \n 29 no live birth \n 19 child death \n Year 2 \n 0 new children measured  \n 158 children lost to follow-up \n 35 moved \n 3 absent \n 72 withdrew \n 29 no live birth \n 19 child death ', size=2.5) +
  annotate('text', x= 1, y=48,label= 'Follow-up', size=3.3) +
  geom_rect(xmin = 9, xmax=25, ymin=19, ymax=31, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 17, y=26,label= '
Year 1 \n 68 clusters \n 376 children \n Year 2 \n 68 clusters \n 358 children ', size=2.5) +
  annotate('text', x= 2.5, y=26,label= 'Subsample \n Enrollment', size=3.5) +
  geom_rect(xmin = 9, xmax=25, ymin=10, ymax=18, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 17, y=15,label= paste(
"Year 1 \n ", 376-y1_ctrl_n, " missing outcome \n Year 2 \n ", 358-y2_ctrl_n, " missing outcome", sep=""), size=2.5) +
  annotate('text', x= 2.5, y=15,label= 'Specimen \n Collection', size=3.5) +
  annotate('text', x= 2.5, y=4,label= 'Analysis', size=3.5) +
  geom_rect(xmin = 9, xmax=25, ymin=-3, ymax=9, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 17, y=4,label= paste('
Year 1 \n ', y1_ctrl_clusters, ' clusters \n ', y1_ctrl_n, " children \n Year 2 \n ", y2_ctrl_clusters, " clusters \n ", y2_ctrl_n, " children", sep=""), size=2.5) ->
  p
p

p +
  geom_segment(
    x=50, xend=50, y=96, yend=86, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=58, y=91, yend=91, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=17, xend=17, y=76, yend=75, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=17, xend=17, y=63, yend=62, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=17, xend=17, y=32, yend=31, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=17, xend=17, y=19, yend=18, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=17, xend=17, y=10, yend=9, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=88, xend=88, y=76, yend=75, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=88, xend=88, y=63, yend=62, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=88, xend=88, y=32, yend=31, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=88, xend=88, y=19, yend=18, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=88, xend=88, y=10, yend=9, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=30, xend=17, y=85, yend=85, 
    size=0.15, linejoin = "mitre", lineend = "butt") + 
  geom_segment(
    x=17, xend=17, y=85, yend=82, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=70, xend=88, y=85, yend=85, 
    size=0.15, linejoin = "mitre", lineend = "butt") + 
  geom_segment(
    x=88, xend=88, y=85, yend=82, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) ->
  p
p

ggsave(p, file = here("figures/stress_enrollment.tiff"), height=14, width=9)

