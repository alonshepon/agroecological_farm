#a program to read and visualize farm data
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
nut <-read.xlsx("AGRO_farm_dataset_2023-10-20.xlsx","self sufficiency")

protein_national_mass<-c(nut$protein[7],nut$protein[5],nut$protein[3])*100
zinc_national_mass<-c(nut$zinc[7],nut$zinc[5],nut$zinc[3])*100
iron_national_mass<-c(nut$iron[7],nut$iron[5],nut$iron[3])*100
magnesium_national_mass<-c(nut$magnesium[7],nut$magnesium[5],nut$magnesium[3])*100
calcium_national_mass<-c(nut$calcium[7],nut$calcium[5],nut$calcium[3])*100
calcium_national_mass<-c(nut$calcium[7],nut$calcium[5],nut$calcium[3])*100
fats_national_mass<-c(nut$total.fat[7],nut$total.fat[5],nut$total.fat[3])*100
energy_national_mass<-c(nut$energy[7],nut$energy[5],nut$energy[3])*100
carbohydrates_national_mass<-c(nut$carbohydr[7],nut$carbohydr[5],nut$carbohydr[3])*100


protein_national_land<-c(nut$protein[13],nut$protein[11],nut$protein[9])*100
zinc_national_land<-c(nut$zinc[13],nut$zinc[11],nut$zinc[9])*100
iron_national_land<-c(nut$iron[13],nut$iron[11],nut$iron[9])*100
magnesium_national_land<-c(nut$magnesium[13],nut$magnesium[11],nut$magnesium[9])*100
calcium_national_land<-c(nut$calcium[13],nut$calcium[11],nut$calcium[9])*100
calcium_national_land<-c(nut$calcium[13],nut$calcium[11],nut$calcium[9])*100
fats_national_land<-c(nut$total.fat[13],nut$total.fat[11],nut$total.fat[9])*100
energy_national_land<-c(nut$energy[13],nut$energy[11],nut$energy[9])*100
carbohydrates_national_land<-c(nut$carbohydr[13],nut$carbohydr[11],nut$carbohydr[9])*100


#---------draw comparison 
posit<-c("energy","protein","fats","carb.","zinc","iron","magn.","calcium")
comp<-data.frame(protein_national_mass,zinc_national_mass,iron_national_mass,magnesium_national_mass,calcium_national_mass,fats_national_mass,energy_national_mass,carbohydrates_national_mass)
comp<-comp %>%rename(protein=protein_national_mass,zinc=zinc_national_mass,iron=iron_national_mass,magn.=magnesium_national_mass,calcium=calcium_national_mass,fats=fats_national_mass,energy=energy_national_mass,carb.=carbohydrates_national_mass)
comp_1<-comp %>% gather('nutrient','value',1:8)
Scenario=c(rep(c("BAU","MIX","AGRO"), 8))
comp_1$Scenario<-Scenario
comp_1$nutrient <- factor(comp_1$nutrient, levels = posit)
gf_mass<-ggplot(data = comp_1, aes(fill=Scenario,x=nutrient, y=value),color=Scenario) +
  geom_hline(yintercept=100)+
  geom_bar(stat="identity",position="dodge")+
  scale_y_continuous(breaks=seq(0,500,50), labels=seq(0,500,50), expand = expansion(mult = c(0, 0.05)),limits = c(0,500)) + ylab("sufficiency per produce (% RDA or AI)")+theme_light()
gf_mass

comp_land<-data.frame(protein_national_land,zinc_national_land,iron_national_land,magnesium_national_land,calcium_national_land,fats_national_land,energy_national_land,carbohydrates_national_land)
comp_land<-comp_land %>%rename(protein=protein_national_land,zinc=zinc_national_land,iron=iron_national_land,magn.=magnesium_national_land,calcium=calcium_national_land,fats=fats_national_land,energy=energy_national_land,carb.=carbohydrates_national_land)
comp_land_1<-comp_land %>% gather('nutrient','value',1:8)
Scenario=c(rep(c("BAU","MIX","AGRO"), 8))
comp_land_1$Scenario<-Scenario
comp_land_1$nutrient <- factor(comp_land_1$nutrient, levels = posit)
gf_land<-ggplot(data = comp_land_1, aes(fill=Scenario,x=nutrient, y=value),color=Scenario)+
  geom_hline(yintercept=100)+
  geom_bar(stat="identity",position="dodge") +guides(fill=guide_legend(title=NULL))+
  scale_y_continuous(breaks=seq(0,500,50), labels=seq(0,500,50),expand = expansion(mult = c(0, 0.05)),limits = c(0,500)) + ylab("sufficiency per area (% RDA or AI) ") +theme_light()
gf_land

prow <- plot_grid(gf_mass+theme(legend.position="none"),
                   gf_land+theme(legend.position="none"),
                   align = 'v',
                   labels = c("A", "B"),
                   hjust = -4,
                   vjust=2,
                   nrow = 1
)
prow
legend_b <- get_legend(gf_land + theme(legend.position="bottom"))

# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).
p <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))
p
#pii<-pii+theme(legend.position="bottom")
#pii<-grid.arrange(arrangeGrob(gf_mass,gf_land, ncol = 2))

ggsave("nutrition_mass_land.png", plot = p, width = 8, height = 5,bg = "white")
ggsave("nutrition_mass_land.tiff", plot = p, width = 8, height = 5,unit='in',bg = "white")