
##################################
# 
# Author: Sofia Gil-Clavel
# 
# Acknowledgments:
#    - Tyler Bruefach
#    - Feinuo Sun
#    - Jennifer Suilteanu
# 
# Date: February 16th, 2022.
# 
# Description: Code to replicate graphs used for the second CAnD3 Briefing Note:
#   "ASSESSING HOW CVD IMPACTS ON CANADIANS’ WELLBEING DEPEND ON SEX AND GEOGRAPHY"
# 
# Computer Environment:
#   - Windows 
#   - R - 4.1.1 (2021)
#   - Rstudio (1.4.1717)
#   - Microsoft Windows 10 Enterprise
# 
# R Packages:
#   - geofacet (0.2.0) 
#   - ggplot2 (3.3.5)
#   - tidyr (1.1.3)
#   - dplyr (1.0.7)
# 
##################################

library(geofacet)
library(ggplot2)
library(tidyr)
library(dplyr)

rm(list=ls())
gc()

#### Codes to merge data with grid ####
# Hand-made database generated to match the Canadian Community Health Survey, 2014
# provinces and the geofacet package canadian grid.
GRID_CODES<-read.csv("PROCESSED_DATA/CODES_4_GRID2.csv")

#### Canadian grid:
"ca_prov_grid1"

# Aggregating some of the North provinces:
new_ca_prov_grid1<-ca_prov_grid1
new_ca_prov_grid1[1,]<-c(1,2,"YT/NT/NU","YUKON/NWT/NUNA")
new_ca_prov_grid1<-new_ca_prov_grid1[-c(2,3),]

# Merging with GRID_CODES
new_ca_prov_grid1<-new_ca_prov_grid1%>%
  right_join(GRID_CODES)

# Removing some columns and renaming provinces names
new_ca_prov_grid1<-new_ca_prov_grid1[,-c(4,6,7)]
colnames(new_ca_prov_grid1)[4]<-"name"

# Removing some columns and renaming provinces names
GRID_CODES<-GRID_CODES[,-2]
colnames(GRID_CODES)[2]<-"name"

# Open the Canadian Community Health Survey, 2014
DATA<-read.csv("DATA/cchs-82M0013-E-2014-Annual-component_F1.csv")

# Codes is a hand-made csv with some variables and codes in the:
# Canadian Community Health Survey, 2014: Annual Component
CODES<-read.csv("DATA/Variable_Labels.csv")

UNIQ_CODES<-unique(CODES$VARIABLE)

DATA2<-DATA[,c("WTS_M",UNIQ_CODES)]

# Transforming into factors
for(cc in UNIQ_CODES){
  ROWS<-which(CODES$VARIABLE==cc)
  DATA2[,cc]<-factor(DATA2[,cc],
                     levels = CODES$VALUE[ROWS],
                     labels = CODES$LABEL[ROWS])
  DATA2[,cc]<-droplevels(DATA2[,cc])
}


## The codes for Yukon, Northwest, and Nunavut will repeat
DATA2<-DATA2%>%
  right_join(GRID_CODES)

### Graph ####

# DHH_SEX: Sex
# GEN_01: Self-perceived health
# CCC_071: Has high blood pressure

DATA2%>%
  group_by(DHH_SEX,GEN_01,CCC_121)%>%
  filter(!GEN_01%in%c(" DON'T KNOW ", " REFUSAL "," NOT STATED "),
         !CCC_121%in%c(" DON'T KNOW ", " REFUSAL "," NOT STATED "))%>%
  count(wt = WTS_M)%>%
  ungroup()%>%
  group_by(DHH_SEX,CCC_121)%>%
  spread(DHH_SEX,n)%>%
  mutate(men=` MALE `/sum(` MALE `,na.rm = TRUE),
         women=` FEMALE `/sum(` FEMALE `,na.rm = TRUE))%>%
  mutate(ratio=round(100*men)/round(100*women))%>%
  ggplot() +
  geom_tile(aes(GEN_01,paste(CCC_121),fill=log(ratio)))+
  coord_flip()+
  geom_text(aes(GEN_01,paste(CCC_121),
                label=paste0(round(100*men),"% / ",round(100*women),"%")))+
  theme_bw()+
  scale_x_discrete(limits=rev)+
  scale_fill_gradient2()+
  labs(y="heart disease",x="Self-perceived health")+
  guides(fill=guide_colorbar(title="log(M/W)"))

# Save the graph
ggsave("PROCESSED_DATA/IMAGES/CVD_MF_ratio.png",
       width = 20,height = 10,units = "cm")

### Plots by Geo

DATA2%>%
  group_by(code,name,DHH_SEX,GEN_01,CCC_121)%>%
  filter(!GEN_01%in%c(" DON'T KNOW ", " REFUSAL "," NOT STATED "),
         !CCC_121%in%c(" DON'T KNOW ", " REFUSAL "," NOT STATED "))%>%
  count(wt = WTS_M)%>%
  ungroup()%>%
  group_by(code,name,DHH_SEX,CCC_121)%>%
  spread(DHH_SEX,n)%>%
  mutate(men=` MALE `/sum(` MALE `,na.rm = TRUE),
         women=` FEMALE `/sum(` FEMALE `,na.rm = TRUE))%>%
  mutate(ratio=round(100*men)/round(100*women))%>%
  ggplot() +
  geom_tile(aes(GEN_01,paste(CCC_121),fill=log(ratio)))+
  coord_flip()+
  geom_text(aes(GEN_01,paste(CCC_121),
                label=paste0(round(100*men),"%/ ",round(100*women),"%")))+
  theme_bw()+
  scale_x_discrete(limits=rev)+
  facet_geo(~ name,grid = new_ca_prov_grid1)+
  scale_fill_gradient2()+
  theme(text = element_text(size = 13))+
  labs(y="heart disease",x="Self-perceived health")+
  guides(fill=guide_colorbar(title="log(M/W)"))

# Save the graph
ggsave("PROCESSED_DATA/IMAGES/CVD_MF_GEO_ratio.png",
       width = 45,height = 23,units = "cm")


