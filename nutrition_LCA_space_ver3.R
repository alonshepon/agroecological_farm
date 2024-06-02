#a program to read and visualize nutrition and LCA data in common space
rm(list = ls(all = TRUE))    #delete variables


library("openxlsx")      #load libraries
library("rlang")
library("ggplot2")
library("dplyr")
library("tidyverse")
library("data.table")
library("stringr")
library("egg")
library("viridis")  #color palettes
library("cowplot")
library("ggpubr")
library("ggh4x")
#-------------------------change working directory
setwd("/Users/alonshepon/My Drive/Main/Research topics/Alternative food production systems/Mediterranean agroforestry model/R code")

#-----------------------read dataset
data_set_farm <-read.xlsx("AGRO_farm_dataset_2023-10-20.xlsx", "Comparative analysis")
data_set_farm_1<-data_set_farm %>% filter(item %in% c("AGRO scores per farm produce","MIX scores per farm produce","BAU scores per farm produce","AGRO scores per farm area","MIX scores per farm area","BAU scores per farm area")) %>%pivot_longer(!item, names_to = "attri", values_to = "count")

#--------------------------
met<-c("produce","produce","produce","area","area","area")
cou<-c("AGRO","MIX","BAU","AGRO","MIX","BAU")
pal_con<-c("#F8766D","green3","cornflowerblue")

#subfigure 1
data_set_farm_2<-data_set_farm_1 %>% filter(attri %in% c("GHG","Terrest.acidificat","Blue.water","Biodiversity.loss","Land.Occupation","Marine.Eutroph","relative.nutritional.score","relative.environmental.score","dev.relative.nutritional.score","dev.relative.environmental.score"))%>%pivot_wider(names_from = attri, values_from = count)
Relative_scores<-data_set_farm_2%>%mutate(GHG=(100*GHG),Acidification=(Terrest.acidificat*100),Eutrophication=(Marine.Eutroph*100),Land=(Land.Occupation*100),Water=(Blue.water*100),Total=(relative.environmental.score*100),Nutrition=(relative.nutritional.score*100),dev.nutrition=(relative.nutritional.score*100),dev.environment=relative.environmental.score*100)%>% select(GHG,Acidification,Eutrophication,Land,Water,Total,Nutrition,dev.environment,dev.nutrition)
Relative_scores$farm<-cou
Relative_scores$method<-met
Relative_scores_1<-Relative_scores%>%pivot_longer(cols =c(1,2,3,4,5,6))
colnames(Relative_scores_1)[6] ="impact"
Relative_scores_p<-Relative_scores_1%>%filter(method=="produce")
Relative_scores_p$dev.environment[c(1,2,3,4,5,7,8,9,10,11,13,14,15,16,17,18)]=NA

GR <- ggplot(Relative_scores_p, aes(y=Nutrition, x=value,color=farm,shape=impact))+
  scale_shape_manual(values=c(15,16,17,18,14,20))+
  ylim(50,200)+
  xlim(-50,450)+
  geom_vline(xintercept = 100,alpha=0.5)+
  geom_hline(yintercept = 100,alpha=0.5)+
  geom_point(size=4) + theme_bw()+labs(x="relative enviromental impacts (%)",y="relative nutritional score (%)")+
  geom_errorbar(aes(ymin=Nutrition-dev.nutrition, ymax=Nutrition+dev.nutrition), width=abs(5), size=0.3, alpha=0.6)+
  geom_errorbar(aes(xmin=value-dev.environment, xmax=value+dev.environment),width=2, size=0.3,alpha=0.6)+
  theme(axis.ticks.y = element_blank(),legend.position = "none",plot.margin = margin(r = 0.01, l = 0.01))+
  annotate("text", x=-47, y=200, label= "A",size=5,col='black',fontface = "bold") 
GR

Relative_scores_area<-Relative_scores_1%>%filter(method=="area")
Relative_scores_area$dev.environment[c(1,2,3,4,5,7,8,9,10,11,13,14,15,16,17,18)]=NA
GA <- ggplot(Relative_scores_area, aes(y=Nutrition, x=value,color=farm,shape=impact))+
  scale_shape_manual(values=c(15,16,17,18,14,20))+
  ylim(50,200)+
  xlim(-20,250)+
  geom_vline(xintercept = 100,alpha=0.5)+
  geom_hline(yintercept = 100,alpha=0.5)+
  geom_point(size=4) + theme_bw()+labs(x="relative enviromental impacts (%)",y="")+
  geom_errorbar(aes(ymin=Nutrition-dev.nutrition, ymax=Nutrition+dev.nutrition), width=abs(2), size=0.3, alpha=0.6)+
  geom_errorbar(aes(xmin=value-dev.environment, xmax=value+dev.environment),width=2, size=0.3,alpha=0.6)+
  theme(axis.ticks.y = element_blank(),axis.text.y=element_blank(), plot.margin = margin(r = 0.01, l = 0.01))+ 
  annotate("text", x=-17, y=200, label= "B",size=5,col='black',fontface = "bold")    
GA
  
#---------arrange per produce and per area into one plot

prow <- plot_grid(GR,
                  GA,
                  rel_widths=c(0.72,1),
                  align = 'h',
                  hjust = -3.5,
                  #labels = c("A", "B"),
                  vjust=1.2,
                  nrow = 1
)
prow

ggsave("nutrition_LCA_space_ver3.png",plot = last_plot(),height = 4, width = 8, dpi = 400,bg = "white") 
ggsave("nutrition_LCA_space_ver3.tiff",plot = last_plot(),unit='in',height = 4, width = 8, dpi = 300,bg = "white") 

