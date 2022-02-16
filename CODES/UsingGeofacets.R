
library(geofacet)
library(ggplot2)
library(tidyr)
library(dplyr)

rm(list=ls())
gc()

#### Codes to merge data with grid ####

GRID_CODES<-read.csv("PROCESSED_DATA/CODES_4_GRID2.csv")

#### Canadian grid
"ca_prov_grid1"

# new_ca_prov_grid1<-read.csv("PROCESSED_DATA/CODES_4_GRID3.csv")

new_ca_prov_grid1<-ca_prov_grid1
new_ca_prov_grid1[1,]<-c(1,2,"YT/NT/NU","YUKON/NWT/NUNA")
new_ca_prov_grid1<-new_ca_prov_grid1[-c(2,3),]

new_ca_prov_grid1<-new_ca_prov_grid1%>%
  right_join(GRID_CODES)

new_ca_prov_grid1<-new_ca_prov_grid1[,-c(4,6,7)]
colnames(new_ca_prov_grid1)[4]<-"name"

#### Open the Data ####
GRID_CODES<-GRID_CODES[,-2]
colnames(GRID_CODES)[2]<-"name"

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

# From those that have a heart disease
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

ggsave("PROCESSED_DATA/IMAGES/CVD_MF_GEO_ratio.png",
       width = 45,height = 23,units = "cm")