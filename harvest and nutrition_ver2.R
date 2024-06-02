#a program to read and visualize harvest and nutrition farm data
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
#-------------------------change working directory
setwd("/Users/alonshepon/Google Drive/My Drive/Main/Research topics/Alternative food production systems/Mediterranean agroforestry model/R code")

#-----------------------read dataset
ff<-"AGRO_farm_dataset_2023-10-20.xlsx"
harvest <-read.xlsx(ff, "Harvest")

#----------------------------------


harvest1 <- harvest %>% select(c('item/month','food_group','jan','feb','march','april','may','june','july','aug','sep','oct','nov','dec'))%>%filter(row_number() <= n()-2)
harvest1[is.na(harvest1)] <- 0
#gather into food groups
harvest2<-harvest1 %>%group_by(food_group) %>% summarise(jan=sum(jan),feb=sum(feb),march=sum(march),april=sum(april),may=sum(may),june=sum(june),july=sum(july),aug=sum(aug),sep=sum(sep),oct=sum(oct),nov=sum(nov),dec=sum(dec))

harvest_pie<-harvest2 %>% group_by(food_group)%>% mutate(per=jan+feb+march+april+may+june+july+aug+sep+oct+nov+dec) %>% select(food_group,per) 
all_harvest<-sum(harvest_pie$per)
harvest_pie2<-harvest_pie %>% summarize(per1=per/all_harvest*100) %>% mutate(labels = scales::percent(per1/100))
harvest3<-harvest2%>%gather("month","kg",2:13);#("jan","feb","march","april","may","june","july","aug","sep","oct","nov","dec")
harvest3$month <- as.integer(factor(harvest3$month, levels = c("jan","feb","march","april","may","june","july","aug","sep","oct","nov","dec")))
harvest3$food_group<-factor(harvest3$food_group)

#----------------------------plot the area stacked area chart

e<-ggplot(harvest3, aes(x=month, y=kg, fill=food_group))+
  geom_area()+labs(x="month",y="harvest (kg)")+theme_bw()+ scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE)+labs(fill = "food groups")+coord_cartesian(xlim = c(1.5, 11.5), ylim = c(5, 200))+
  scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12"),labels=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),limits=c("1","2","3","4","5","6","7","8","9","10","11","12"))
   e                
p<-ggplot(harvest_pie2, aes(x="", y=per1, fill=food_group)) +
     geom_bar(stat="identity", width=1) +
     coord_polar("y", start=0)+ scale_color_viridis(discrete = TRUE, option = "D")+theme_linedraw(base_size = 10)+
  scale_fill_viridis(discrete = TRUE)+labs(title = "Total harvest (%)",x='',y='')+theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


plot.with.inset <-
  ggdraw() +
  draw_plot(e) +
  draw_plot(p, x = 0.45, y = .50, width = .45, height = .45)
plot.with.inset
ggsave("pic.png",height = 5, width = 7, dpi = 320) 
ggsave("pic.tiff",height = 5, width = 7, dpi = 300,units='in',bg = 'white') 

#------------------------------treemap

tree1<-harvest%>%select(c('item/month','food_group','yearly.total.(Kg)'))%>%filter(row_number() <= n()-2)

library(treemapify)
colnames(tree1)[1] <- 'food'
colnames(tree1)[2] <- 'groups'
colnames(tree1)[3] <- 'value'
w<-ggplot(tree1, aes(fill = groups, area = value, label = food, subgroup=groups))+
geom_treemap(start = "bottomleft")+scale_fill_viridis(discrete = TRUE, option = "D")+
geom_treemap_text(colour = c(rep("white", 5),
                             rep("black", 24),
                             rep("white", 1),1,rep("white", 1),1,rep("white", 2),1,rep("white", 2),1,1,1,1),
                  place = "center",
                  grow = FALSE, reflow = T,size = 25)+
  guides(fill = guide_legend(title = "food groups"))+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=15)) #change legend text font size
w
                                                          
ggsave("treemap.png",height = 7, width = 10, dpi = 450) 


#--------------nutritional sufficency analysis

#protein
setwd("/Users/alonshepon/Google Drive/My Drive/Main/Research topics/Alternative food production systems/Mediterranean agroforestry model/R code")

protein <-read.xlsx(ff,"protein")
#protein<-protein[,c(1,2,4,6,5,7,8,9,3,10,11,12,13)]
RDA_protein<-56
protein_1 <-protein %>% mutate (vegetable = vegetable/RDA_protein*100,oil = oil/RDA_protein*100,cereal=cereal/RDA_protein*100,sugar=sugar/RDA_protein*100,legumes=legumes/RDA_protein*100,tuber=tuber/RDA_protein*100,fruits=fruits/RDA_protein*100)
protein_2<-protein_1 %>% select(c('month','vegetable','oil','cereal','sugar','legumes','tuber','fruits'))%>%filter(row_number() <= n()-2)
colnames(protein_2)[1] <- 'foods'
Mon=c(rep(c("1","2","3","4","5","6","7","8","9","10","11","12"), 7))
protein_3<-protein_2 %>% gather('foods','perc',2:8)
protein_3$Mo<-Mon
protein_3$Mo <- factor(protein_3$Mo[1:12], levels = protein_3$Mo[1:12])
#protein_3$foods2<-factor(protein_3$foods,levels=c('cereal','legumes','oil','sugar','tuber','fruits','vegetable'))
prot<-ggplot(protein_3, aes(x=Mo, y=perc,fill=foods)) +
  geom_bar(stat='identity', position = position_stack(reverse = TRUE) )+
  scale_color_viridis(discrete = TRUE, option = "D")+theme_linedraw(base_size = 10)+
  scale_fill_viridis(discrete = TRUE)+labs(title = "",x='',y='protein (% of RDA)')+theme(legend.position = "none")
prot

legend_b <- get_legend(
 prot + 
    guides(fill = guide_legend(nrow = 1)) +
   theme(legend.position = "bottom",legend.title=element_blank()))

#-----------------------zinc
zinc <-read.xlsx(ff,"zinc")
RDA_zinc<-8
zinc_1 <-zinc %>% mutate (vegetables = vegetable/RDA_zinc*100,oil = oil/RDA_zinc*100,cereals=cereal/RDA_zinc*100,sugar=sugar/RDA_zinc*100,legumes=legumes/RDA_zinc*100,tubers=tubers/RDA_zinc*100,fruits=fruits/RDA_zinc*100)
zinc_2<-zinc_1 %>% select(c('month','vegetables','oil','cereals','sugar','legumes','tubers','fruits'))%>%filter(row_number() <= n()-2)
colnames(zinc_2)[1] <- 'foods'
Mon=c(rep(c("1","2","3","4","5","6","7","8","9","10","11","12"), 7))
zinc_3<-zinc_2 %>% gather('foods','perc',2:8)
zinc_3$Mo<-Mon
zinc_3$Mo <- factor(zinc_3$Mo[1:12], levels = zinc_3$Mo[1:12])

zin<-ggplot(zinc_3, aes(x=Mo, y=perc,fill=foods)) +
  geom_bar(stat="identity",position = position_stack(reverse = TRUE))+
  scale_color_viridis(discrete = TRUE, option = "D")+theme_linedraw(base_size = 10)+
  scale_fill_viridis(discrete = TRUE)+labs(title = "",x='',y='zinc (% of AI)')+theme(legend.position = "none")

#-----------------------iron
iron <-read.xlsx(ff,"iron")
RDA_iron<-8
iron_1 <-iron %>% mutate (vegetables = vegetable/RDA_iron*100,oil = oil/RDA_iron*100,cereals=cereal/RDA_iron*100,sugar=sugar/RDA_iron*100,legumes=legumes/RDA_iron*100,tubers=tubers/RDA_iron*100,fruits=fruits/RDA_iron*100)
iron_2<-iron_1 %>% select(c('month','vegetables','oil','cereals','sugar','legumes','tubers','fruits'))%>%filter(row_number() <= n()-2)
colnames(iron_2)[1] <- 'foods'
Mon=c(rep(c("1","2","3","4","5","6","7","8","9","10","11","12"), 7))
iron_3<-iron_2 %>% gather('foods','perc',2:8)
iron_3$Mo<-Mon
iron_3$Mo <- factor(iron_3$Mo[1:12], levels = iron_3$Mo[1:12])
iro<-ggplot(iron_3, aes(x=Mo, y=perc,fill=foods)) +
  geom_bar(stat="identity",position = position_stack(reverse = TRUE))+
  scale_color_viridis(discrete = TRUE, option = "D")+theme_linedraw(base_size = 10)+
  scale_fill_viridis(discrete = TRUE)+labs(title = "",x='',y='iron (% of AI)')+theme(legend.position = "none")
iro


#-----------------------magnesium
magnesium<-read.xlsx(ff,"magnesium")
RDA_magnesium<-420
magnesium_1 <-magnesium %>% mutate (vegetables = vegetable/RDA_magnesium*100,oil = oil/RDA_magnesium*100,cereals=cereal/RDA_magnesium*100,sugar=sugar/RDA_magnesium*100,legumes=legumes/RDA_magnesium*100,tubers=tubers/RDA_magnesium*100,fruits=fruits/RDA_magnesium*100)
magnesium_2<-magnesium_1 %>% select(c('month','vegetables','oil','cereals','sugar','legumes','tubers','fruits'))%>%filter(row_number() <= n()-2)
colnames(magnesium_2)[1] <- 'foods'
Mon=c(rep(c("1","2","3","4","5","6","7","8","9","10","11","12"), 7))
magnesium_3<-magnesium_2 %>% gather('foods','perc',2:8)
magnesium_3$Mo<-Mon
magnesium_3$Mo <- factor(magnesium_3$Mo[1:12], levels = magnesium_3$Mo[1:12])

magn<-ggplot(magnesium_3, aes(x=Mo, y=perc,fill=foods)) +
  geom_bar(stat="identity",position = position_stack(reverse = TRUE))+
  scale_color_viridis(discrete = TRUE, option = "D")+theme_linedraw(base_size = 10)+
  scale_fill_viridis(discrete = TRUE)+labs(title = "",x='',y='magnesium (% of AI)')+theme(legend.position = "none")
magn

#-----------------------calcium

calcium<-read.xlsx(ff,"calcium")
RDA_calcium<-1000
calcium_1 <-calcium %>% mutate (vegetables = vegetable/RDA_calcium*100,oil = oil/RDA_calcium*100,cereals=cereal/RDA_calcium*100,sugar=sugar/RDA_calcium*100,legumes=legumes/RDA_calcium*100,tubers=tubers/RDA_calcium*100,fruits=fruits/RDA_calcium*100)
calcium_2<-calcium_1 %>% select(c('month','vegetables','oil','cereals','sugar','legumes','tubers','fruits'))%>%filter(row_number() <= n()-2)
colnames(calcium_2)[1] <- 'foods'
Mon=c(rep(c("1","2","3","4","5","6","7","8","9","10","11","12"), 7))
calcium_3<-calcium_2 %>% gather('foods','perc',2:8)
calcium_3$Mo<-Mon
calcium_3$Mo <- factor(calcium_3$Mo[1:12], levels = calcium_3$Mo[1:12])

calc<-ggplot(calcium_3, aes(x=Mo, y=perc,fill=foods)) +
  geom_bar(stat="identity",position = position_stack(reverse = TRUE))+
  scale_color_viridis(discrete = TRUE, option = "D")+theme_linedraw(base_size = 10)+
  scale_fill_viridis(discrete = TRUE)+labs(title = "",x='',y='calcium (% of AI)')+theme(legend.position = "none")
calc

#-----------------------fats
fats<-read.xlsx(ff,"fats")
RDA_fats<-73
fats_1 <-fats %>% mutate (vegetables = vegetable/RDA_fats*100,oil = oil/RDA_fats*100,cereals=cereal/RDA_fats*100,sugar=sugar/RDA_fats*100,legumes=legumes/RDA_fats*100,tubers=tubers/RDA_fats*100,fruits=fruits/RDA_fats*100)
fats_2<-fats_1 %>% select(c('month','vegetables','oil','cereals','sugar','legumes','tubers','fruits'))%>%filter(row_number() <= n()-2)
colnames(fats_2)[1] <- 'foods'
Mon=c(rep(c("1","2","3","4","5","6","7","8","9","10","11","12"), 7))
fats_3<-fats_2 %>% gather('foods','perc',2:8)
fats_3$Mo<-Mon
fats_3$Mo <- factor(fats_3$Mo[1:12], levels = fats_3$Mo[1:12])

fa<-ggplot(fats_3, aes(x=Mo, y=perc,fill=foods)) +
  geom_bar(stat="identity",position = position_stack(reverse = TRUE))+
  scale_color_viridis(discrete = TRUE, option = "D")+theme_linedraw(base_size = 10)+
  scale_fill_viridis(discrete = TRUE)+labs(title = "",x='',y='fats (% of RDA)')+theme(legend.position = "none")
fa

#-----------------------energy
energy<-read.xlsx(ff,"calories")
RDA_energy<-2180
energy_1 <-energy %>% mutate (vegetables = vegetable/RDA_energy*100,oil = oil/RDA_energy*100,cereals=cereal/RDA_energy*100,sugar=sugar/RDA_energy*100,legumes=legumes/RDA_energy*100,tubers=tubers/RDA_energy*100,fruits=fruits/RDA_energy*100)
energy_2<-energy_1 %>% select(c('month','vegetables','oil','cereals','sugar','legumes','tubers','fruits'))%>%filter(row_number() <= n()-2)
colnames(energy_2)[1] <- 'foods'
Mon=c(rep(c("1","2","3","4","5","6","7","8","9","10","11","12"), 7))
energy_3<-energy_2 %>% gather('foods','perc',2:8)
energy_3$Mo<-Mon
energy_3$Mo <- factor(energy_3$Mo[1:12], levels = calcium_3$Mo[1:12])

eng<-ggplot(energy_3, aes(x=Mo, y=perc,fill=foods)) +
  geom_bar(stat="identity",position = position_stack(reverse = TRUE))+
  scale_color_viridis(discrete = TRUE, option = "D")+theme_linedraw(base_size = 10)+
  scale_fill_viridis(discrete = TRUE)+labs(title = "",x='',y='energy (% of RDA)')+theme(legend.position = "none")
eng

#-----------------------carbonhydrates
carbonhydrates<-read.xlsx(ff,"carbohydrates")
RDA_carb<-130
carbonhydrates_1 <-carbonhydrates %>% mutate (vegetables = vegetable/RDA_carb*100,oil = oil/RDA_carb*100,cereals=cereal/RDA_carb*100,sugar=sugar/RDA_carb*100,legumes=legumes/RDA_carb*100,tubers=tubers/RDA_carb*100,fruits=fruits/RDA_carb*100)
carbonhydrates_2<-carbonhydrates_1 %>% select(c('month','vegetables','oil','cereals','sugar','legumes','tubers','fruits'))%>%filter(row_number() <= n()-2)
colnames(carbonhydrates_2)[1] <- 'foods'
Mon=c(rep(c("1","2","3","4","5","6","7","8","9","10","11","12"), 7))
carbonhydrates_3<-carbonhydrates_2 %>% gather('foods','perc',2:8)
carbonhydrates_3$Mo<-Mon
carbonhydrates_3$Mo <- factor(carbonhydrates_3$Mo[1:12], levels = fats_3$Mo[1:12])


carb<-ggplot(carbonhydrates_3, aes(x=Mo, y=perc,fill=foods)) +
  geom_bar(stat="identity",position = position_stack(reverse = TRUE))+
  scale_color_viridis(discrete = TRUE, option = "D")+theme_linedraw(base_size = 10)+
  scale_fill_viridis(discrete = TRUE)+labs(title = "",x='',y='carbs. (% of RDA)')+theme(legend.position = "none")
carb


pii<-grid.arrange(arrangeGrob(eng,carb,prot,fa,zin,iro, magn, calc, ncol = 4))
nut1<-grid.arrange(pii, legend_b, nrow = 2, heights = c(10, 1))
nut1
ggsave("nut.png", plot = nut1, width = 8, height = 5)
ggsave("nut.tiff",plot = nut1,dpi=300,width = 8,height = 5,units='in',bg = 'white')
