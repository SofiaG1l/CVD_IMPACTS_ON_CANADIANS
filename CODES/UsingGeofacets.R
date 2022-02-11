
##################################
# 
# Author: Sofia Gil-Clavel
#
# Acknowledgments: 
#       Tyler Bruefach
#       Feinuo Sun, and 
#       Jennifer Suilteanu
#       
# Date: Last update February 11th, 2022
# 
# Description: Code to create the graphs used for the 
#              second CAnD3 Briefing Note:
#              "ASSESSING HOW CVD IMPACTS ON CANADIANS’ 
#                WELLBEING DEPEND ON SEX AND GEOGRAPHY"
# 
# Computer Environment:
#   - Windows 
#   - R version 4.1.1 (2021-08-10)
#   - Rstudio (1.4.1717)
#   - Microsoft Windows 10 Enterprise
# 
# R packages version:
#   - geofacet (0.2.0)
#   - ggplot (3.3.5)
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

#### Canadian grid
"ca_prov_grid1"

# new_ca_prov_grid1<-read.csv("PROCESSED_DATA/CODES_4_GRID3.csv")

new_ca_prov_grid1<-ca_prov_grid1
new_ca_prov_grid1[1,]<-c(1,2,"YT/NT/NU","YUKON/NWT/NUNA")
new_ca_prov_grid1<-new_ca_prov_grid1[-c(2,3),]


#### Codes to merge data with grid ####

GRID_CODES<-read.csv("PROCESSED_DATA/CODES_4_GRID2.csv")

#### Open the Data ####
DATA<-read.csv("DATA/CCHS - CSV/cchs-82M0013-E-2014-Annual-component_F1.csv")

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

DATA2$GEOGPRV2=DATA$GEOGPRV
DATA2$GEODPMF2=DATA$GEODPMF

## The codes for Yukon, Northwest, and Nunavut will repeat
DATA2<-DATA2%>%
  right_join(GRID_CODES)

### Graph ####

DATA2%>%
  group_by(code,DHH_SEX,DHHGMS)%>%
  filter(!DHHGMS%in%c(" DON'T KNOW ", " REFUSAL "," NOT STATED "))%>%
  count(wt = WTS_M)%>%
  group_by(code)%>%
  mutate(per=n/sum(n))%>%
  ggplot(aes(DHHGMS,per,fill=DHH_SEX)) +
  geom_col(position = position_dodge()) +
  facet_geo(~ code, grid = new_ca_prov_grid1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

UNIQ_CODES

# From those that have a heart disease
# CCC_071: Has high blood pressure
DATA2%>%
  group_by(code,name,DHH_SEX,GEN_01,CCC_121,CCC_071)%>%
  filter(!GEN_01%in%c(" DON'T KNOW ", " REFUSAL "," NOT STATED "),
         !CCC_121%in%c(" DON'T KNOW ", " REFUSAL "," NOT STATED "),
         !CCC_071%in%c(" DON'T KNOW ", " REFUSAL "," NOT STATED "))%>%
  count(wt = WTS_M)%>%
  ungroup()%>%
  group_by(code,name,DHH_SEX)%>%
  spread(DHH_SEX,n)%>%
  mutate(ratio=` MALE `/` FEMALE `)%>%
  ggplot() +
  geom_tile(aes(GEN_01,paste0(CCC_121,CCC_071),fill=log(ratio)))+
  coord_flip()+
  facet_geo(~ code,grid = new_ca_prov_grid1)+
  theme_bw()+
  scale_x_discrete(limits=rev)+
  scale_fill_distiller(palette = "RdBu",
                       direction = -1)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y="heart disease/ Blood pressure",x="Self-perceived health")+
  guides(fill=guide_colorbar(title="ratio"))

# From those that have a heart disease without facet_geo
DATA2%>%
  group_by(code,name,DHH_SEX,GEN_01,CCC_121)%>%
  filter(!GEN_01%in%c(" DON'T KNOW ", " REFUSAL "," NOT STATED "),
         !CCC_121%in%c(" NO "," DON'T KNOW ", " REFUSAL "," NOT STATED "))%>%
  count(wt = WTS_M)%>%
  ungroup()%>%
  group_by(code,name,DHH_SEX)%>%
  mutate(per=n/sum(n))%>%
  ggplot() +
  geom_col(aes(GEN_01,per))+
  facet_grid(DHH_SEX~name)+
  theme_bw()+
  scale_x_discrete(limits=rev)+
  scale_fill_distiller(palette = "Spectral",
                       direction = -1)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y="Sex",x="Self-perceived health")+
  guides(fill=guide_colorbar(title="%"))

# From those that have a heart disease and immigrant
DATA2%>%
  group_by(code,DHH_SEX,GEN_01,CCC_121,SDCFIMM)%>%
  filter(!GEN_01%in%c(" DON'T KNOW ", " REFUSAL "," NOT STATED "),
         !SDCFIMM%in%c(" DON'T KNOW ", " REFUSAL "," NOT STATED "),
         !CCC_121%in%c(" NO "," DON'T KNOW ", " REFUSAL "," NOT STATED "))%>%
  count(wt = WTS_M)%>%
  ungroup()%>%
  group_by(code,DHH_SEX,SDCFIMM)%>%
  mutate(per=n/sum(n))%>%
  ggplot() +
  geom_tile(aes(GEN_01,paste0(DHH_SEX,"/","Immig:",SDCFIMM),
                fill=100*per))+
  coord_flip()+
  facet_geo(~ code,grid = new_ca_prov_grid1)+
  theme_bw()+
  scale_x_discrete(limits=rev)+
  scale_fill_distiller(palette = "Spectral",
                       direction = -1)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y="Sex",x="Self-perceived health")+
  guides(fill=guide_colorbar(title="%"))

# without Facet_Geo
DATA2%>%
  group_by(code,name,DHH_SEX,GEN_01,CCC_121,SDCFIMM)%>%
  filter(!GEN_01%in%c(" DON'T KNOW ", " REFUSAL "," NOT STATED "),
         !SDCFIMM%in%c(" DON'T KNOW ", " REFUSAL "," NOT STATED "),
         !CCC_121%in%c(" NO "," DON'T KNOW ", " REFUSAL "," NOT STATED "))%>%
  count(wt = WTS_M)%>%
  ungroup()%>%
  group_by(code,name,DHH_SEX,SDCFIMM)%>%
  mutate(per=n/sum(n))%>%
  ggplot() +
  geom_tile(aes(GEN_01,SDCFIMM,
                fill=100*per))+
  coord_flip()+
  facet_grid(DHH_SEX~name)+
  theme_bw()+
  scale_x_discrete(limits=rev)+
  scale_fill_distiller(palette = "Spectral",
                       direction = -1)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y="Sex",x="Self-perceived health")+
  guides(fill=guide_colorbar(title="%"))
