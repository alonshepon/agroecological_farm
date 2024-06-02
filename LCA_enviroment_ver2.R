#a program to read and visualize the LCA results of the farm
rm(list = ls(all = TRUE))    #delete variables


library("openxlsx")      #load libraries
library("rlang")
library("ggplot2")
library("dplyr")
library("tidyverse")
library("data.table")
library("stringr")
library("egg")
library("viridis")  #color palletes
library("cowplot")
library("ggpubr")
library("devtools")

#-------------------------change working directory
setwd("/Users/alonshepon/My Drive/Main/Research topics/Alternative food production systems/Mediterranean agroforestry model/R code")

#-----------------------read dataset

data_set_farm <-read.xlsx("AGRO_farm_dataset_2023-10-20.xlsx", "Comparative analysis")
data_set_farm_1<-data_set_farm %>% filter(item %in% c("AGRO outputs per farm produce","MIX outputs per farm produce","BAU outputs per farm produce")) %>%pivot_longer(!item, names_to = "attri", values_to = "count")
data_set_farm_2<-data_set_farm_1 %>% filter(attri %in% c("GHG","Terrest.acidificat","Blue.water","Marine.Eutroph","Human.toxicity","Biodiversity.loss","Land.Occupation","Eco.toxicity")) %>%pivot_wider(names_from = attri, values_from = count)
LCA_scores<-data_set_farm_2
LCA_scores <- LCA_scores[ -c(1) ]
LCA_scores<-LCA_scores%>%mutate(GHG=(GHG),Eutro.=(Marine.Eutroph),Acid.=(Terrest.acidificat),Hum.Tox=(Human.toxicity),Biodiver.=(Biodiversity.loss),Land=(Land.Occupation),Water=(Blue.water),Eco.Tox=(Eco.toxicity))%>% select(GHG, Eutro.,Acid.,Hum.Tox,Biodiver.,Land,Water,Eco.Tox)
LCA_scores$group<-c("AGRO","MIX","BAU")


data_set_farm_area <-read.xlsx("AGRO_farm_dataset_2023-10-20.xlsx", "Comparative analysis")
data_set_farm_1_area <-data_set_farm_area  %>% filter(item %in% c("AGRO outputs per farm area","MIX outputs per farm area","BAU outputs per farm area")) %>%pivot_longer(!item, names_to = "attri", values_to = "count")
data_set_farm_2_area<-data_set_farm_1_area %>% filter(attri %in% c("GHG","Terrest.acidificat","Blue.water","Marine.Eutroph","Human.toxicity","Biodiversity.loss","Land.Occupation","Eco.toxicity")) %>%pivot_wider(names_from = attri, values_from = count)
LCA_scores_area<-data_set_farm_2_area
LCA_scores_area <- LCA_scores_area[ -c(1)]
LCA_scores_area<-LCA_scores_area%>%mutate(GHG=(GHG),Eutro.=(Marine.Eutroph),Acid.=(Terrest.acidificat),Hum.Tox=(Human.toxicity),Biodiver.=(Biodiversity.loss),Land=(Land.Occupation),Water=(Blue.water),Eco.Tox=(Eco.toxicity))%>% select(GHG, Eutro.,Acid.,Hum.Tox,Biodiver.,Land,Water,Eco.Tox)
LCA_scores_area$group<-c("AGRO","MIX","BAU")


#------------------plot as horizontal bar plots

#---------per produce
posit<-c("Water","Acid.","Hum.Tox","GHG","Eutro.","Eco.Tox","Biodiver.","Land")
LCA_scores_1<-LCA_scores %>% gather('cat','val',1:8)
LCA_scores_1$cat <- factor(LCA_scores_1$cat, levels = posit)

#--GHG
LCA_scores_2<-LCA_scores_1%>%filter(cat=='GHG')
gf_GHG<-ggplot(data =LCA_scores_2, aes(y=cat,x=val,fill=group)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  scale_x_continuous(breaks=c(0,0.5*LCA_scores_2$val[3],1*LCA_scores_2$val[3],1.5*LCA_scores_2$val[3],2*LCA_scores_2$val[3],2.5*LCA_scores_2$val[3],3*LCA_scores_2$val[3]), labels=round(c(0,0.5*LCA_scores_2$val[3],1*LCA_scores_2$val[3],1.5*LCA_scores_2$val[3],2*LCA_scores_2$val[3],2.5*LCA_scores_2$val[3],3*LCA_scores_2$val[3])), expand = expansion(mult = c(0, 0.0)),limits = c(0,1100)) + ylab("")+
  xlab(bquote(GHG~(kg~CO[2]*-eq)))+
  theme_light()+
  theme(text = element_text(size = 15),axis.title.x=element_text(size=13),axis.text.y=element_blank(),legend.position="none",panel.grid.minor = element_line(size = 0.5), panel.grid.major = element_line(size = 1))
gf_GHG

#--Biodiversity

#LCA_scores_2<-LCA_scores_1%>%filter(cat=='Biodiver.')

#gf_bio<-ggplot(data =LCA_scores_2, aes(y=cat,x=val,fill=group)) +
#  geom_bar(position = 'dodge', stat = 'identity')+
#  scale_x_continuous(breaks=c(0,0.5*LCA_scores_2$val[3],1*LCA_scores_2$val[3],1.5*LCA_scores_2$val[3],2*LCA_scores_2$val[3],2.5*LCA_scores_2$val[3],3*LCA_scores_2$val[3]), labels=(signif(c(0,0.5*LCA_scores_2$val[3],1*LCA_scores_2$val[3],1.5*LCA_scores_2$val[3],2*LCA_scores_2$val[3],2.5*LCA_scores_2$val[3],3*LCA_scores_2$val[3]),digits = 1)), expand = expansion(mult = c(0, 0.0)),limits = c(0,7*10^(-5))) +
#  xlab(bquote("biodiversity loss"~("species-yr")))+
#  theme_light()+ ylab("")+
#  theme(text = element_text(size = 15),axis.title.x=element_text(size=13),axis.text.y=element_blank(),legend.position="none")
#gf_bio
#LCA_scores_2<-LCA_scores_1%>%filter(cat=='Biodiver.')

#--Eutro.
LCA_scores_2<-LCA_scores_1%>%filter(cat=="Eutro.")
gf_bio<-ggplot(data =LCA_scores_2, aes(y=cat,x=val,fill=group)) +
geom_bar(position = 'dodge', stat = 'identity')+
scale_x_continuous(breaks=c(0,0.5*LCA_scores_2$val[3],1*LCA_scores_2$val[3],1.5*LCA_scores_2$val[3],2*LCA_scores_2$val[3],2.5*LCA_scores_2$val[3],3*LCA_scores_2$val[3]), labels=(signif(c(0,0.5*LCA_scores_2$val[3],1*LCA_scores_2$val[3],1.5*LCA_scores_2$val[3],2*LCA_scores_2$val[3],2.5*LCA_scores_2$val[3],3*LCA_scores_2$val[3]),digits = 1)), expand = expansion(mult = c(0, 0.0)),limits = c(0,5)) +
xlab(bquote("eutrophication"~(kg~N*-eq)))+
theme_light()+ ylab("")+
theme(text = element_text(size = 15),axis.title.x=element_text(size=13),axis.text.y=element_blank(),legend.position="none",panel.grid.minor = element_line(size = 0.5), panel.grid.major = element_line(size = 1))
gf_bio

#--Land

LCA_scores_2<-LCA_scores_1%>%filter(cat=='Land')

gf_land<-ggplot(data =LCA_scores_2, aes(y=cat,x=val,fill=group)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  scale_x_continuous(breaks=c(0,0.5*LCA_scores_2$val[3],1*LCA_scores_2$val[3],1.5*LCA_scores_2$val[3],2*LCA_scores_2$val[3],2.5*LCA_scores_2$val[3],3*LCA_scores_2$val[3],3.5*LCA_scores_2$val[3],4*LCA_scores_2$val[3]), labels=signif(0.001*c(0,0.5*LCA_scores_2$val[3],1*LCA_scores_2$val[3],1.5*LCA_scores_2$val[3],2*LCA_scores_2$val[3],2.5*LCA_scores_2$val[3],3*LCA_scores_2$val[3],3.5*LCA_scores_2$val[3],4*LCA_scores_2$val[3]),digits = 2), expand = expansion(mult = c(0, 0.0)),limits = c(0,6000))+
  xlab(bquote("land occupation"~(m^2*a/1000)))+
  theme_light()+ ylab("")+
  theme(text = element_text(size = 15),axis.title.x=element_text(size=13),axis.text.y=element_blank(),legend.position="none",panel.grid.minor = element_line(size = 0.5), panel.grid.major = element_line(size = 1))
gf_land

#--acidification

LCA_scores_2<-LCA_scores_1%>%filter(cat=='Acid.')

gf_acid<-ggplot(data =LCA_scores_2, aes(y=cat,x=val,fill=group)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  scale_x_continuous(breaks=c(0,0.5*LCA_scores_2$val[3],1*LCA_scores_2$val[3]), labels=signif(c(0,0.5*LCA_scores_2$val[3],1*LCA_scores_2$val[3]),digits = 2), expand = expansion(mult = c(0, 0.0)),limits = c(-2,5))+
  xlab(bquote("acidifcation"~(kg~SO[2]*-eq)))+
  theme_light()+ ylab("")+
  theme(text = element_text(size = 15),axis.title.x=element_text(size=13),axis.text.y=element_blank(),legend.position="none",panel.grid.minor = element_line(size = 0.5), panel.grid.major = element_line(size = 1))
gf_acid

#--Water

LCA_scores_2<-LCA_scores_1%>%filter(cat=='Water')

gf_water<-ggplot(data =LCA_scores_2, aes(y=cat,x=val,fill=group)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  scale_x_continuous(breaks=c(0,0.5*LCA_scores_2$val[3],1*LCA_scores_2$val[3]), labels=signif(c(0,0.5*LCA_scores_2$val[3],1*LCA_scores_2$val[3]),digits = 3), expand = expansion(mult = c(0, 0.0)),limits = c(0,250))+
  xlab(bquote("blue water "(m^3)))+
  theme_light()+ ylab("")+
  theme(text = element_text(size = 15),axis.title.x=element_text(size=13),axis.text.y=element_blank(),legend.position="none",panel.grid.minor = element_line(size = 0.5), panel.grid.major = element_line(size = 1))
gf_water

#arrange per produce into one column
#pii<-grid.arrange(arrangeGrob(gf_GHG,gf_bio,gf_water,gf_acid,gf_land))
nut1<-grid.arrange(gf_GHG,gf_bio,gf_water,gf_acid,gf_land, nrow = 5,ncol = 1)
nut1

#---------per area

posit<-c("Water","Acid.","Hum.Tox","GHG","Eutro.","Eco.Tox","Biodiver.","Land")
LCA_scores_1_area<-LCA_scores_area %>% gather('cat','val',1:8)
LCA_scores_1_area$cat <- factor(LCA_scores_1_area$cat, levels = posit)

#--GHG
LCA_scores_2_area<-LCA_scores_1_area%>%filter(cat=='GHG')
gf_GHG_area<-ggplot(data =LCA_scores_2_area, aes(y=cat,x=val,fill=group)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  scale_x_continuous(breaks=c(0,0.5*LCA_scores_2_area$val[3],1*LCA_scores_2_area$val[3]), labels=round(c(0,0.5*LCA_scores_2_area$val[3],1*LCA_scores_2_area$val[3])), expand = expansion(mult = c(0, 0.0)),limits = c(0,950)) + ylab("")+
  xlab(bquote(GHG~(kg~CO[2]*-eq)))+
  theme_light()+
  theme(text = element_text(size = 15),axis.title.x=element_text(size=13),axis.text.y=element_blank(),legend.position="none",panel.grid.minor = element_line(size = 0.5), panel.grid.major = element_line(size = 1))
gf_GHG_area

#--Biodiversity

#LCA_scores_2_area<-LCA_scores_1_area%>%filter(cat=='Biodiver.')

#gf_bio_area<-ggplot(data =LCA_scores_2_area, aes(y=cat,x=val,fill=group)) +
#  geom_bar(position = 'dodge', stat = 'identity')+
#  scale_x_continuous(breaks=c(0,0.5*LCA_scores_2_area$val[3],1*LCA_scores_2_area$val[3],1.5*LCA_scores_2_area$val[3]), labels=(signif(c(0,0.5*LCA_scores_2_area$val[3],1*LCA_scores_2_area$val[3],1.5*LCA_scores_2_area$val[3]),digits = 1)), expand = expansion(mult = c(0, 0.0)),limits = c(0,7*10^(-5))) +
#  xlab(bquote("biodiversity loss"~("species-yr")))+
#  theme_light()+ ylab("")+
#  theme(text = element_text(size = 15),axis.title.x=element_text(size=13),axis.text.y=element_blank(),legend.position="none")
#gf_bio_area

#--Eutrophication

LCA_scores_2_area<-LCA_scores_1_area%>%filter(cat=='Eutro.')

gf_bio_area<-ggplot(data =LCA_scores_2_area, aes(y=cat,x=val,fill=group)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  scale_x_continuous(breaks=c(0,0.5*LCA_scores_2_area$val[3],1*LCA_scores_2_area$val[3],1.5*LCA_scores_2_area$val[3]), labels=(signif(c(0,0.5*LCA_scores_2_area$val[3],1*LCA_scores_2_area$val[3],1.5*LCA_scores_2_area$val[3]),digits = 1)), expand = expansion(mult = c(0, 0.0)),limits = c(0,4.5)) +
  xlab(bquote("eutrophication"~(kg~N*-eq)))+
  theme_light()+ ylab("")+
  theme(text = element_text(size = 15),axis.title.x=element_text(size=13),axis.text.y=element_blank(),legend.position="none",panel.grid.minor = element_line(size = 0.5), panel.grid.major = element_line(size = 1))
gf_bio_area

#--Land

LCA_scores_2_area<-LCA_scores_1_area%>%filter(cat=='Land')

gf_land_area<-ggplot(data =LCA_scores_2_area, aes(y=cat,x=val,fill=group)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  scale_x_continuous(breaks=c(0,0.5*LCA_scores_2_area$val[3],1*LCA_scores_2_area$val[3],1.5*LCA_scores_2_area$val[3],2*LCA_scores_2_area$val[3],2.5*LCA_scores_2_area$val[3],3*LCA_scores_2_area$val[3],3.5*LCA_scores_2_area$val[3],4*LCA_scores_2_area$val[3]), labels= signif(0.001*c(0,0.5*LCA_scores_2_area$val[3],1*LCA_scores_2_area$val[3],1.5*LCA_scores_2_area$val[3],2*LCA_scores_2_area$val[3],2.5*LCA_scores_2_area$val[3],3*LCA_scores_2_area$val[3],3.5*LCA_scores_2_area$val[3],4*LCA_scores_2_area$val[3]),digits = 2), expand = expansion(mult = c(0, 0.0)),limits = c(0,6000))+
  xlab(bquote("land occupation"~(m^2*a/1000)))+
  theme_light()+ ylab("")+
  theme(text = element_text(size = 15),axis.title.x=element_text(size=13),axis.text.y=element_blank(),legend.position="none",panel.grid.minor = element_line(size = 0.5), panel.grid.major = element_line(size = 1))
gf_land_area

#--acidification

LCA_scores_2_area<-LCA_scores_1_area%>%filter(cat=='Acid.')

gf_acid_area<-ggplot(data =LCA_scores_2_area, aes(y=cat,x=val,fill=group)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  scale_x_continuous(breaks=c(0,0.5*LCA_scores_2_area$val[3],1*LCA_scores_2_area$val[3]), labels=signif(c(0,0.5*LCA_scores_2_area$val[3],1*LCA_scores_2_area$val[3]),digits = 2), expand = expansion(mult = c(0, 0.0)),limits = c(-2,8))+
  xlab(bquote("acidifcation"~(kg~SO[2]*-eq)))+
  theme_light()+ ylab("")+
  theme(text = element_text(size = 15),axis.title.x=element_text(size=13),axis.text.y=element_blank(),legend.position="none",panel.grid.minor = element_line(size = 0.5), panel.grid.major = element_line(size = 1))
gf_acid_area

#--Water

LCA_scores_2_area<-LCA_scores_1_area%>%filter(cat=='Water')

gf_water_area<-ggplot(data =LCA_scores_2_area, aes(y=cat,x=val,fill=group)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  scale_x_continuous(breaks=c(0,0.5*LCA_scores_2_area$val[3],1*LCA_scores_2_area$val[3]), labels=signif(c(0,0.5*LCA_scores_2_area$val[3],1*LCA_scores_2_area$val[3]),digits = 3), expand = expansion(mult = c(0, 0.0)),limits = c(0,400))+
  xlab(bquote("blue water "(m^3)))+
  theme_light()+ ylab("")+
  theme(text = element_text(size = 15),axis.title.x=element_text(size=13),axis.text.y=element_blank(),legend.position="none")
gf_water_area

#arrange per produce into one column
#pii<-grid.arrange(arrangeGrob(gf_GHG,gf_bio,gf_water,gf_acid,gf_land))
nut2<-grid.arrange(gf_GHG_area,gf_bio_area,gf_water_area,gf_acid_area,gf_land_area, nrow = 5,ncol = 1)
nut2
#---------arrange per produce and per area into one plot

prow <- plot_grid(nut1,
                  nut2,
                  rel_widths=c(1,1),
                  align = 'h',
                  hjust = -1,
                  labels=c("A","B"),
                  vjust=1.2,
                  nrow = 1
)
prow
legend_b <- get_legend(gf_land + theme(legend.position="bottom",title = element_blank()))
p <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))
p
ggsave("envir_lca.png",dpi=500,width = 8,height = 8,bg = 'white')
ggsave("lca_bar.tiff",dpi=300,width = 8,height = 8,units='in',bg = 'white')
