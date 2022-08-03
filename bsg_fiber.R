rm(list=ls()) 


library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("patchwork")
library(patchwork)


#importing data
bsg_fiber <-read.csv("C:\\Users\\dpeguero\\Desktop\\ammonia pretreatment\\bsg_fiber_data.csv", header= TRUE,
                     sep= ',')

bsg_fiber

df <- data.frame(bsg_fiber)
  
colnames(bsg_fiber)
  
#combine data into one column and changing name
bsgNH3 <-
  bsg_fiber  %>%

   gather(8:10, key="lignocellulose", value="value" )

bsgNH3 %>%
  
  mutate(parameter=factor(parameter),
         substrate= factor(substrate),
         sample_name=factor(sample_name),
         treatment=factor(treatment),
         time=factor(time),
         dose= factor(dose),
         replicate=factor(replicate),
         lignocellulose=factor(lignocellulose),
         value=factor(value))

#mutate treatment into one treatment condition and save it as new bsgNH3

bsgNH3 %>%
  mutate(treatment_condition = case_when(treatment=="raw" ~"raw",
                                 treatment=="NH3"& dose==0~"Control",
                                 treatment== "NH3" & dose==1 ~ "NH3_1",
                                 treatment== "NH3" & dose==5~ "NH3_5")) -> bsgNH3
#set order of treatment condition

bsgNH3$treatment_condition <- factor(bsgNH3$treatment_condition, levels = c("raw", "Control", "NH3_1", "NH3_5"))
bsgNH3$lignocellulose<- factor(bsgNH3$lignocellulose, levels = c("NDF", "ADF", "Hemicellulose"))

colnames(bsgNH3)

#plot ggplot bsg control, 

bsgNH3 %>%
  filter(!treatment_condition=="raw") %>%
  ggplot(aes(treatment_condition,value))+
  
#modify color and shape of time plus making title  
  geom_point(aes(color=factor(lignocellulose), 
                 shape= factor(time)), size= 2)+
  labs(x="",
       y= "% DM",
       title = "BSG")+
  
  theme_bw()+
  
#separating figures based on pretreatment time
  facet_wrap(~time) +
  
# how to change and adjust the title and change legend  
  theme(plot.title = element_text(hjust=0.5))+
  scale_color_discrete(name="Lignocellulosic composition") +
  scale_shape_discrete(name="Pretreatment time")-> p1

p1
 
 

#plot 1 is bsg so that i can arrange



#end of BSG data plot


#import cow manure data

cm_fiber <- read.csv("C:\\Users\\dpeguero\\Desktop\\ammonia pretreatment\\cm_fiber.csv")

cmNH3 <- 
  cm_fiber  %>%
  
  gather(8:10, key="lignocellulose", value="value" )
  
#combine data into one column and changing name
cmNH3 %>%

  mutate(parameter=factor(parameter),
         substrate= factor(substrate),
         sample_name=factor(sample_name),
         treatment=factor(treatment),
         time=factor(time),
         dose= factor(dose),
         replicate=factor(replicate),
         lignocellulose=factor(lignocellulose),
         value=factor(value))
#mutate treatment into  treatment condition and save it as new cmNH3

cmNH3 %>%
  mutate(treatment_condition = case_when(treatment=="raw" ~"raw",
                                         treatment=="NH3"& dose==0~"Control",
                                         treatment== "NH3" & dose==1 ~ "NH3_1",
                                         treatment== "NH3" & dose==5~ "NH3_5")) -> cmNH3
cmNH3
 
#set order of treatment condition for cm

cmNH3$treatment_condition <- factor(cmNH3$treatment_condition, levels = c("raw", "Control", "NH3_1", "NH3_5"))
cmNH3$lignocellulose<- factor(cmNH3$lignocellulose, levels = c("NDF", "ADF", "Hemicellulose"))


#plot ggplot cm control, 

cmNH3 %>%
  filter(!treatment_condition=="raw") %>%
  ggplot(aes(treatment_condition,value))+
  
  #modify color and shape of time plus making title  
  geom_point(aes(color=factor(lignocellulose), 
                 shape= factor(time)), size= 2)+
  labs(x="",
       y= "% DM",
       title = "CM")+
  
  theme_bw()+
  
  #separating figures based on pretreatment time
  facet_wrap(~time) +
  
  # how to change and adjust the title and change legend  
  theme(plot.title = element_text(hjust=0.5))+
  
  theme(legend.position = "none")-> p2

p2 +
  ylim(0,60) ->p2

p2 #plot 2 is cm so that i can arrange

#end of cm data plot


#arrnging ggplot combining multiple plots

p1/p2





















