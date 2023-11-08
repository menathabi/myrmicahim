setwd("C:/Users/abira/OneDrive/Desktop/MS thesis 2023 datas and papers/Final Thesis Analysis")


library("png")
library("vegan")
library(wesanderson)
library(ggplot2)
library("png")
library("vegan")
library(scales)
library(tidyverse)
library(hrbrthemes)
library(viridis)



## Locations v/s Number of nests 
library(readxl)
NN <- read_excel("C:/Users/abira/OneDrive/Desktop/MS thesis 2023 datas and papers/Final Thesis Analysis/MS Thesis 2022-23 field collection _ Manali (2).xlsx", 
                                                           sheet = "Nest number")
View(NN)
library(ggplot2)
library("png")
library("vegan")
library(scales)
library(tidyverse)
library(hrbrthemes)
library(viridis)
#cbPalette <- c()
#plot
#scale_fill_brewer(palette = "RdYlBu")
#scale_fill_viridis(discrete = TRUE)
#scale_fill_manual(values = c("#C7F9EE","#6DF0D2","#1DE4BD","#1AC9E6","#1AC9E6","#19AADE","#176BA0"))

NNP<-ggplot(NN,aes(fill=Species, y=Number, x=Location))+
  geom_bar(position = "stack", stat = "identity")

NNP + scale_color_viridis(discrete = TRUE)+
  theme_ipsum()+
  ylab("Number of nests")+
  xlab("Locations")+
  #scale_fill_brewer(palette = "PuRd")+
  scale_fill_manual(values = c("#C7F9EE","#6DF0D2","#1DE4BD","#66FFFF","#1AC9E6","#19AADE","#176BA0"))+
  #scale_fill_manual(values = c("#FCEAE6","#F0A58F","#EA7369","#EB548C","#DB4CB2","#AF4BCE","#7D3AC1"))+
  scale_y_continuous(breaks = seq(0,35,by=2))+
  #scale_fill_discrete("Species",labels=c(expression(italic("Myrmica aimonissabaudiae")),expression(italic("Myrmica cachmiriensis")),expression(italic("Myrmica hecate")),expression(italic("Myrmica inezae")),expression(italic("Myrmica sp.")),expression(italic("Myrmica wardi")),"Non-myrmica"))+
  theme(axis.title.x.bottom = element_text(color="#142459",hjust = 0.5, size=10, face="bold",family = "TT Times New Roman"),
        axis.title.y = element_text(color="#142459", size=10,hjust = 0.5, face="bold",family ="TT Times New Roman"),
        axis.text.x = element_text(size=9, angle=0,family ="TT Times New Roman",color = "black" ,face = "bold"),
        axis.text.y = element_text(color= "black"),
        panel.background = element_rect(fill = "#FFFFFF",colour = "#142459",size = 2),
        legend.title = element_text(color="#142459",size = 10,face = "bold"),
        legend.title.align = 0.5,
        legend.background = (element_rect(colour = "#142459")),
        legend.text = element_text(color ="#000000",size = 9,face = "bold.italic", family = "TT Times New Roman"))

### Anthropization
library(plot3D)
library(RColorBrewer)
library(ggplot2)


library(readxl)
AA <- read_excel("MS Thesis 2022-23 field collection _ Manali (2).xlsx", 
                                                           sheet = "anthropization")
View(AA)


library(extrafont)
AAP <-ggplot(AA, aes(fill=Location, x=Location,y=sum))+
  geom_bar(stat = "identity")

AAP+ ylab("Degree of Human Interference")+
  xlab("Locations")+
  #scale_fill_brewer(palette = "PuRd")+
  scale_fill_manual(values = c("#6DF0D2","#1DE4BD","#66FFFF","#1AC9E6","#19AADE","#176BA0"))+
  #scale_fill_manual(values = c("#FCEAE6","#F0A58F","#EA7369","#EB548C","#DB4CB2","#AF4BCE","#7D3AC1"))+
  scale_y_continuous(breaks = seq(0,28,by=2))+
  #scale_fill_discrete("Species",labels=c(expression(italic("Myrmica aimonissabaudiae")),expression(italic("Myrmica cachmiriensis")),expression(italic("Myrmica hecate")),expression(italic("Myrmica inezae")),expression(italic("Myrmica sp.")),expression(italic("Myrmica wardi")),"Non-myrmica"))+
  theme(axis.title.x.bottom = element_text(color="#142459",hjust = 0.5, size=10, face="bold",family = "TT Times New Roman"),
        axis.title.y = element_text(color="#142459", size=10,hjust = 0.5, face="bold",family ="TT Times New Roman"),
        axis.text.x = element_text(size=9, angle=0,family ="TT Times New Roman" ),
        panel.background = element_rect(fill = "#FFFFFF",colour = "#142459",size = 2))
        #legend.text = element_text(color ="#000000",size = 9,face = "italic", family = "TT Times New Roman"))



#### correlation of human interference and myrmica species richness of locations chosen
library(readxl)
HR<- read_excel("MS Thesis 2022-23 field collection _ Manali (2).xlsx", 
                                                           sheet = "anthropization")
View(HR)
library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)

## Most basic line chart (re-ordering in ascending order)
HI<- ggplot(HR,aes(x = reorder(Location, +Human_interference), y=Human_interference))+
  geom_bar(color="#1AC9E6",fill="#66FFFF",  size=1, stat = "identity")
HR<- HI +  geom_line(aes(x = Location, y=Myrmica_Species_richness), stat = "identity", color="#142459", size=2)
HR + ggtitle("Human interference: range 1-25")+
  theme_ipsum()


############# Behavioural session ###############

## Recruitment strategies
library(readxl)
R <- read_excel("MS Thesis 2022-23 field collection _ Manali (2).xlsx", 
                                                           sheet = "Recruitment")
View(R)

RP<-ggplot(R,aes(fill=Recruitment, y=Proportion, x=Species))+
  geom_bar(position = "stack", stat = "identity",width = 0.7)+
  coord_flip()

RP + scale_color_viridis(discrete = TRUE)+
  theme_ipsum()+
  ylab("Proportion of recruitment strategies followed by Myrmica species")+
  xlab("Myrmica species")+
  #scale_fill_brewer(palette = "PuRd")+
  scale_fill_manual(values = c("#66FFFF","#1AC9E6","#19AADE","#176BA0"))+
  #scale_fill_manual(values = c("#FCEAE6","#F0A58F","#EA7369","#EB548C","#DB4CB2","#AF4BCE","#7D3AC1"))+
  scale_y_continuous(breaks = seq(0,36,by=4))+
  #scale_fill_discrete("Species",labels=c(expression(italic("Myrmica aimonissabaudiae")),expression(italic("Myrmica cachmiriensis")),expression(italic("Myrmica hecate")),expression(italic("Myrmica inezae")),expression(italic("Myrmica sp.")),expression(italic("Myrmica wardi")),"Non-myrmica"))+
  theme(axis.title.x.bottom = element_text(color="#142459",hjust = 0.5, size=10, face="bold",family = "TT Times New Roman"),
        axis.title.y = element_text(color="#142459", size=10,hjust = 0.5, face="bold",family ="TT Times New Roman"),
        axis.text.y = element_text(size=9,face = "bold.italic",color = "black", angle=0,family ="TT Times New Roman" ),
        axis.text.x = element_text(color = "black", face = "bold"),
        panel.background = element_rect(fill = "#FFFFFF",colour = "#142459",size = 2),
        legend.title = element_text(color="#142459",size = 10,face = "bold"),
        legend.title.align = 0.5,
        legend.background = (element_rect(colour = "#142459")),
        legend.text = element_text(color ="#000000",size = 7.3,face = "bold", family = "TT Times New Roman"))

######## Bait selection ######

####### 
library(readxl)
B <- read_excel("MS Thesis 2022-23 field collection _ Manali (2).xlsx", 
                                                           sheet = "Bait")
View(B)
BP<-ggplot(B,aes(fill=Food_source, y=Proportion, x=Species))+
  geom_bar(position = "stack", stat = "identity",width = 0.6)

BPP <-BP + scale_color_viridis(discrete = TRUE)+
  theme_ipsum()+
  ylab("Proportion of Myrmica species choosing certain food resource")+
  xlab("Myrmica species")+
  #scale_fill_brewer(palette = "PuRd")+
  scale_fill_manual(values = c("#19AADE","#176BA0","#142459"))+
  #scale_fill_manual(values = c("#FCEAE6","#F0A58F","#EA7369","#EB548C","#DB4CB2","#AF4BCE","#7D3AC1"))+
  scale_y_continuous(breaks = seq(0,1,by=0.25))+
  #scale_fill_discrete("Species",labels=c(expression(italic("Myrmica aimonissabaudiae")),expression(italic("Myrmica cachmiriensis")),expression(italic("Myrmica hecate")),expression(italic("Myrmica inezae")),expression(italic("Myrmica sp.")),expression(italic("Myrmica wardi")),"Non-myrmica"))+
  theme(axis.title.x.bottom = element_text(color="#142459",hjust = 0.5, size=10, face="bold",family = "TT Times New Roman"),
        axis.title.y = element_text(color="#142459", size=10,hjust = 0.5, face="bold",family ="TT Times New Roman"),
        axis.text.y = element_text(size=9,face = "bold.italic",color = "black", angle=0,family ="TT Times New Roman" ),
        axis.text.x = element_text(color = "black", face = "bold"),
        panel.background = element_rect(fill = "#FFFFFF",colour = "#142459",size = 2),
        legend.title = element_text(color="#142459",size = 10,face = "bold"),
        legend.title.align = 0.5,
        legend.background = (element_rect(colour = "#142459")),
        legend.text = element_text(color ="#000000",size = 7.3,face = "bold", family = "TT Times New Roman"))
BPP+ coord_flip()

  
###########        ########        ##########
################### ANOVA TEST  #####################


#### Myrmica aimonissabaudiae

library(readxl)
MAB<- read_excel("MS Thesis 2022-23 field collection _ Manali (2).xlsx", 
                                                           sheet = "ANOVA and post hoc for bait pre")
View(MAB)

MABA <- aov(Ma ~ Bait_type, data = MAB )
summary(MABA)
TukeyHSD(MABA)
plot(TukeyHSD(MABA, conf.level = .95), las=1)

#### Myrmica cachmiriensis

library(readxl)
MCB<- read_excel("MS Thesis 2022-23 field collection _ Manali (2).xlsx", 
                 sheet = "ANOVA and post hoc for bait pre")
View(MCB)

MCBA <- aov(Mc ~ Bait_type, data = MCB )
summary(MCBA)
TukeyHSD(MCBA)
plot(TukeyHSD(MCBA, conf.level = .95), las=1)

#### Myrmica hecate

library(readxl)
MHB<- read_excel("MS Thesis 2022-23 field collection _ Manali (2).xlsx", 
                 sheet = "ANOVA and post hoc for bait pre")
View(MHB)

MHBA <- aov(Mh ~ Bait_type, data = MHB )
summary(MHBA)
TukeyHSD(MHBA)
plot(TukeyHSD(MHBA, conf.level = .95), las=1)

#### Myrmica inezae

library(readxl)
MIB<- read_excel("MS Thesis 2022-23 field collection _ Manali (2).xlsx", 
                 sheet = "ANOVA and post hoc for bait pre")
View(MIB)

MIBA <- aov(Mi ~ Bait_type, data = MIB )
summary(MIBA)
TukeyHSD(MIBA)
plot(TukeyHSD(MIBA, conf.level = .95), las=1)

#### Myrmica wardi

library(readxl)
MWB<- read_excel("MS Thesis 2022-23 field collection _ Manali (2).xlsx", 
                 sheet = "ANOVA and post hoc for bait pre")
View(MWB)

MWBA <- aov(Mw ~ Bait_type, data = MWB )
summary(MWBA)
TukeyHSD(MWBA)
plot(TukeyHSD(MWBA, conf.level = .95), las=1)


##### bait preference plot for each Myrmica species

library(readxl)
BPP<- read_excel("MS Thesis 2022-23 field collection _ Manali (2).xlsx", 
                                                           sheet = "bait preference plot")
View(BPP)

library(extrafont)
BPPP <-ggplot(BPP, aes(fill=Bait, x=Species,y=Proportion))+
  geom_bar(stat = "identity",position = "dodge", width = 0.8)+
  coord_flip()

BPPP+ ylab("Proportion of colonies choosing bait")+
  xlab("Myrmica species")+
  scale_color_viridis(discrete = TRUE)+
  theme_ipsum()+
  #scale_fill_brewer(palette = "PuRd")+
  scale_fill_manual(values = c("#6DF0D2","#1DE4BD","#66FFFF","#1AC9E6","#19AADE","#176BA0"))+
  #scale_fill_manual(values = c("#FCEAE6","#F0A58F","#EA7369","#EB548C","#DB4CB2","#AF4BCE","#7D3AC1"))+
  scale_y_continuous(breaks = seq(0,1.05,by=0.15))+
  #scale_fill_discrete("Species",labels=c(expression(italic("Myrmica aimonissabaudiae")),expression(italic("Myrmica cachmiriensis")),expression(italic("Myrmica hecate")),expression(italic("Myrmica inezae")),expression(italic("Myrmica sp.")),expression(italic("Myrmica wardi")),"Non-myrmica"))+
  theme(axis.title.x.bottom = element_text(color="#142459",hjust = 0.5, size=10, face="bold",family = "TT Times New Roman"),
        axis.title.y = element_text(color="#142459", size=10,hjust = 0.5, face="bold",family ="TT Times New Roman"),
        axis.text.y = element_text(size=9,face = "bold.italic",color = "black", angle=0,family ="TT Times New Roman" ),
        axis.text.x = element_text(color = "black", face = "bold",size = 9),
        panel.background = element_rect(fill = "#FFFFFF",colour = "#142459",size = 2),
        legend.title = element_text(color="#142459",size = 10,face = "bold"),
        legend.title.align = 0.5,
        legend.background = (element_rect(colour = "#142459")),
        legend.text = element_text(color ="#000000",size = 7.3,face = "bold", family = "TT Times New Roman"))

########### Foraging ###########
###### month wise activity of species on foraging ##############

library(readxl)
FHS<- read_excel("MS Thesis 2022-23 field collection _ Manali (2).xlsx", 
                                                           sheet = "Foraging activity across season")
View(FHS)

## ANOVA and posthoc test


# hecate
FHSA <- aov(Proportion ~ Month, data =FHS  )
summary(FHSA)
TukeyHSD(FHSA)
plot(TukeyHSD(FHSA, conf.level = .95), las=3)


# aimonissabaudiae

FASA <- aov(Proportion ~ Month, data =FAS  )
summary(FASA)
TukeyHSD(FASA)
plot(TukeyHSD(FASA, conf.level = .95), las=3)

# cachmiriensis

FCSA <- aov(Proportion ~ Month, data =FCS  )
summary(FCSA)
TukeyHSD(FCSA)
plot(TukeyHSD(FCSA, conf.level = .95), las=3)


# wardi

FWSA <- aov(Proportion ~ Month, data =FWS  )
summary(FWSA)
TukeyHSD(FWSA)
plot(TukeyHSD(FWSA, conf.level = .95), las=3)




##### plot #####

library(extrafont)
FASP <-ggplot(FAS, aes(fill=Month, x=Species,y=Proportion))+
  geom_bar(stat = "identity",position = "dodge", width = 0.8)+
  coord_flip()

FASP+ ylab("Proportion of foragers")+
  xlab("Myrmica species")+
  scale_color_viridis(discrete = TRUE)+
  theme_ipsum()+
  #scale_fill_brewer(palette = "PuRd")+
  scale_fill_manual(values = c("#6DF0D2","#1DE4BD","#66FFFF"))+
  #scale_fill_manual(values = c("#FCEAE6","#F0A58F","#EA7369","#EB548C","#DB4CB2","#AF4BCE","#7D3AC1"))+
  scale_y_continuous(breaks = seq(0,1,by=0.15))+
  #scale_fill_discrete("Species",labels=c(expression(italic("Myrmica aimonissabaudiae")),expression(italic("Myrmica cachmiriensis")),expression(italic("Myrmica hecate")),expression(italic("Myrmica inezae")),expression(italic("Myrmica sp.")),expression(italic("Myrmica wardi")),"Non-myrmica"))+
  theme(axis.title.x.bottom = element_text(color="#142459",hjust = 0.5, size=10, face="bold",family = "TT Times New Roman"),
        axis.title.y = element_text(color="#142459", size=10,hjust = 0.5, face="bold",family ="TT Times New Roman"),
        axis.text.y = element_text(size=9,face = "bold.italic",color = "black", angle=0,family ="TT Times New Roman" ),
        axis.text.x = element_text(color = "black", face = "bold",size = 9),
        panel.background = element_rect(fill = "#FFFFFF",colour = "#142459",size = 2),
        legend.title = element_text(color="#142459",size = 10,face = "bold"),
        legend.title.align = 0.5,
        legend.background = (element_rect(colour = "#142459")),
        legend.text = element_text(color ="#000000",size = 7.3,face = "bold", family = "TT Times New Roman"))



citation()

