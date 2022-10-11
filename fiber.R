rm(list=ls()) #clears the environment 


library(tidyverse)
library(ggplot2)
library(dplyr)
#install.packages("patchwork")
library(patchwork)
library("writexl")

#loading file with all data

all_fiber <-read.csv(file = "data/All_fiber_data_repsincl.csv")



str(all_fiber)
head(all_fiber)

levels(all_fiber$substrate)

#new name cleaning up data making factorize so easier to work with
fiberNH3 <- 
  
  all_fiber %>% 
  
  
  gather(8:12, key="lignocellulose", value="value" ) %>%
  filter(!lignocellulose=="cellulose") %>% 
  filter(!lignocellulose == "ADL") %>% 
  filter(!treatment_condition=="NaOH") %>% 
  filter(!experiment_no=="2") %>% 
  mutate(substrate= factor(substrate),
         sample_name=factor(sample_name),
         treatment_condition=factor(treatment_condition),
         pretreatment_time=factor(pretreatment_time),
         dose= factor(dose),
         replicate=factor(replicate),
         lignocellulose=factor(lignocellulose),
         experiment_no=factor(experiment_no))

fiberNH3$lignocellulose<- factor(fiberNH3$lignocellulose, levels = c("NDF", "ADF", "hemicellulose"))



str(fiberNH3) #this tells me if i have factor and number

levels(fiberNH3$lignocellulose)# lets me know whats in that column...this is to clean data



#fiberNH3 summary of descriptive statistics
fiber_summary <-
  fiberNH3 %>% 
  group_by(substrate,lignocellulose,treatment_condition, pretreatment_time, dose) %>%
  filter(!substrate=="") %>% 
  summarise(mean=mean(value),
            stdev=sd(value),
            max=max(value),
            min=min(value))
#set order
fiber_summary$treatment_condition <- factor(fiber_summary$dose, levels = c("0", "1", "5")) 



fiber_summary
#save fiber summary to excel
#install.packages("writexl")


write_xlsx(fiber_summary, "C:\\Users\\dpeguero\\Desktop\\ammonia pretreatment\\ammonia_r\\data\\fiber_summary.xlsx")

#mutate treatment condition new so that it all has one treatment condition new
fiberNH3 %>%
  mutate(treatment_condition_new = case_when(treatment_condition=="raw" ~"Raw",
                                             treatment_condition=="NH3"& dose==0~"Control",
                                             treatment_condition== "NH3" & dose==1 ~ "1%",
                                             treatment_condition== "NH3" & dose==5~ "5%")) ->fiberNH3

colnames(fiberNH3)

#plot bsg 

#cleaning up bsg

bsg_fiber_all<-
  fiberNH3 %>% 
  filter(substrate== "bsg")

bsgNH3 <-
  bsg_fiber_all

#set order of treatment condition

bsgNH3$treatment_condition_new <- factor(bsgNH3$treatment_condition_new, levels = c("Raw", "Control", "1%", "5%"))




#plot ggplot bsg control, 

bsgNH3 %>%
  filter(!treatment_condition_new=="Raw") %>%
  ggplot(aes(treatment_condition_new,value, color=factor(lignocellulose), 
             shape= factor(pretreatment_time)), size= 1.5)+
  
  #modify color and shape of time plus making title  
  geom_point()+

  labs(x="",
       y= "% DM",
       title = "BSG")+
  facet_wrap(~pretreatment_time) +
  
  
  theme_bw()+
  
  #separating figures based on pretreatment time
 
  # how to change and adjust the title and change legend  
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.text.x = element_text(size=15, color = "black")) +#changing text size for each axis
  
  theme(axis.text.y = element_text(size=15, color="black")) +
  
  theme(axis.title.y = element_text(size = 15, color = "black"))+
  scale_color_discrete(name="Fiber") +
  scale_shape_discrete(name="Pretreatment time")+ theme(legend.position = "bottom")-> p1
p1


#plot all raw substrates 

raw_all <-
  all_fiber %>% 
  
  gather(8:12, key="lignocellulose", value="value" ) %>%
  filter(!treatment_condition=="NaOH") %>% 
  filter(!experiment_no=="2") %>% 
  filter(!treatment_condition=="NH3") %>%
  mutate(substrate= factor(substrate),
         sample_name=factor(sample_name),
         treatment_condition=factor(treatment_condition),
         pretreatment_time=factor(pretreatment_time),
         dose= factor(dose),
         replicate=factor(replicate),
         lignocellulose=factor(lignocellulose),
         experiment_no=factor(experiment_no)) %>% 
  filter(!lignocellulose=="NDF") %>% 
  filter((!lignocellulose=="ADF"))


raw.plot<-
  raw_all %>% 
  
  group_by(substrate,lignocellulose) %>%
  summarise(mean=mean(value),
            stdev=sd(value), .groups = "drop") %>% 
  mutate(substrate_new=case_when(substrate=="bsg" ~ "Spent grain",
                                 substrate== "cm" ~ "Cow manure",
                                 substrate== "odb" ~ "Oat byproduct",
                                 substrate== "gc" ~ "Grass clippings")) %>% 
  mutate(lignocellulose_new=case_when(lignocellulose=="ADL" ~ "Lignin",
                                 lignocellulose== "cellulose" ~ "Cellulose",
                                 lignocellulose== "hemicellulose" ~ "Hemicellulose"))


raw.plot$lignocellulose_new <- factor(raw.plot$lignocellulose_new, levels = c("Cellulose", "Hemicellulose", "Lignin"))



raw.plot %>% 
  ggplot(aes(x=factor(lignocellulose_new),y=mean, fill=lignocellulose_new))+
  geom_bar(position= "dodge", stat = "identity")+
  facet_wrap(~substrate_new) +
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=.2,
                position=position_dodge(.9)) +
  
  theme_bw()+
  xlab("")+
  ylab("% DM")+
  theme(legend.position = " none")+
  scale_fill_grey()+ ylim(0,40)->raw_all_plots
                    

 
raw_all_plots
ggsave("output//fiber_rawdata.jpeg", units = "cm",width = 14,height = 12,dpi = "print")


#plot cm 
cm_fiber_all<-
  fiberNH3 %>% 
  filter(substrate== "cm") 


cmNH3 <-
  cm_fiber_all 

cmNH3

#set order of treatment condition

cmNH3$treatment_condition_new <- factor(cmNH3$treatment_condition_new, levels = c("raw", "Control", "NH3_1", "NH3_5"))


#plot ggplot cm control, 

cmNH3 %>%
  filter(!treatment_condition_new=="raw") %>%
  filter(!lignocellulose=="NA") %>% 
  ggplot(aes(treatment_condition_new,value))+
  
  #modify color and shape of time plus making title  
  geom_point(aes(color=factor(lignocellulose), 
                 shape= factor(pretreatment_time)), size= 1.5)+
  labs(x="",
       y= "% DM",
       title = "CM")+
  
  theme_bw()+
  
  #separating figures based on pretreatment time
  facet_wrap(~pretreatment_time) +
  
  # how to change and adjust the title and change legend  
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.text.x = element_text(size=15, color = "black")) +#changing text size for each axis
  
  theme(axis.text.y = element_text(size=15, color="black")) +
  
  theme(axis.title.y = element_text(size = 15, color = "black"))+
  scale_color_discrete(name="Fiber") +
  scale_shape_discrete(name="Pretreatment time")+ theme(legend.position = "bottom")->p2
p2
#plot gc

gc_fiber_all<-
  fiberNH3 %>% 
  filter(substrate== "gc") %>% 
  

gcNH3<-
  gc_fiber_all 

#set order of treatment condition

gcNH3$treatment_condition_new <- factor(gcNH3$treatment_condition_new, levels = c("raw", "Control", "NH3_1", "NH3_5"))


#plot ggplot gc control, 

gcNH3 %>%
  filter(!treatment_condition_new=="raw") %>%
  ggplot(aes(treatment_condition_new,value))+
  
  #modify color and shape of time plus making title  
  geom_point(aes(color=factor(lignocellulose), 
                 shape= factor(pretreatment_time)), size= 1.5)+
  labs(x="",
       y= "% DM",
       title = "GC")+
  
  theme_bw()+
  
  #separating figures based on pretreatment time
  facet_wrap(~pretreatment_time) +
  
  # how to change and adjust the title and change legend  
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.text.x = element_text(size=15, color = "black")) +#changing text size for each axis
  
  theme(axis.text.y = element_text(size=15, color="black")) +
  
  theme(axis.title.y = element_text(size = 15, color = "black"))+
  scale_color_discrete(name="Fiber") +
  scale_shape_discrete(name="Pretreatment time")+ theme(legend.position = "bottom")->p3
p3

#filter for odb only 

odb_fiber_all<-
  fiberNH3 %>% 
  filter(substrate== "odb")  
  
  
odbNH3<-
  odb_fiber_all

#plot ggplot odb control, 

odbNH3 %>%
  filter(!treatment_condition_new=="raw") %>%
  ggplot(aes(treatment_condition_new,value))+
  
  #modify color and shape of time plus making title  
  geom_point(aes(color=factor(lignocellulose), 
                 shape= factor(pretreatment_time)), size=1.5)+
  labs(x="",
       y= "% DM",
       title = "ODB")+
  
  theme_bw()+
  
  #separating figures based on pretreatment time
  facet_wrap(~pretreatment_time) +
  
  # how to change and adjust the title and change legend  
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.text.x = element_text(size=15, color = "black")) +#changing text size for each axis
  
  theme(axis.text.y = element_text(size=15, color="black")) +
  
  theme(axis.title.y = element_text(size = 15, color = "black"))+
  scale_color_discrete(name="Fiber") +
  scale_shape_discrete(name="Pretreatment time")+ theme(legend.position = "bottom")+
  ylim(0,60) -> p4
p4

ggsave("output/ODB1.jpeg", units = "cm", width = 14, height = 12, dpi = "print")

