library(tidyverse)
library(ggrepel)
library(ggplot2)
library(cowplot)

setwd("C:/Users/charlotte.spencer/OneDrive - West Point/Desktop/School/AY21-2/MA499")

styku_data<-read.csv("STYKU_data_3FEB-16FEB.csv")
gender_xNum <- read.csv("Gender_xNumber.csv")

styku_data <- styku_data%>%
  left_join(select(gender_xNum,Gender, x.Number..include.the..x..),by = c("Profile.Full.Name" = "x.Number..include.the..x.."))%>%
  unique()

colnames(styku_data)

### boxplots of different measurements
# men vs women

h_w <- styku_data%>%
  select(Gender, Scan.Height, Weight)%>%
  na.omit()%>%
  ggplot(aes(x = Weight, y = Scan.Height, color = Gender)) + 
  geom_point()+
  labs(title = "Height v Weight",
       y = "Height (in)") +
  theme(plot.title = element_text(hjust = 0.5))
h_w


# Legs
legs <- styku_data%>%
  select(Gender, Calf.Right, Mid.Thigh.Right, Right.Leg.Volume, 
         Right.Leg.Surface.Area, Thigh.Right.Lower, Thigh.Right.Upper)%>%
  na.omit()%>%
  group_by(Gender)

calf <- ggplot(data = legs,aes(x = Gender, y = Calf.Right)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 1, size = 3)+
  labs(title = "Calf",
       y = "Single Leg Calf Circumference") + 
  theme(plot.title = element_text(hjust = 0.5))

Leg <- legs%>%
  select(Gender, Right.Leg.Volume, Right.Leg.Surface.Area)%>%
  group_by(Gender)%>%
  summarise(LegSA = mean(Right.Leg.Surface.Area),
            LegVol = mean(Right.Leg.Volume))%>%
  pivot_longer(!Gender,names_to = "Measurement", values_to = "Mean")%>%
  ggplot(aes(x = Gender, y = Mean, fill = Measurement))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(title = "Single Leg M
       easurements")+
  theme(plot.title = element_text(hjust = 0.5))

thighs <- legs%>%
  select(Gender, Mid.Thigh.Right, Thigh.Right.Lower, Thigh.Right.Upper)%>%
  group_by(Gender)%>%
  summarise(MidThigh = mean(Mid.Thigh.Right),
            LowerThigh = mean(Thigh.Right.Lower),
            Upperthigh = mean(Thigh.Right.Upper))%>%
  pivot_longer(!Gender,names_to = "Measurement", values_to = "Mean")%>%
  ggplot(aes(x = Gender, y = Mean, fill = Measurement))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(title = "Thigh Circumferences")+
  theme(plot.title = element_text(hjust = 0.5))

  
#Arms
arms <- styku_data%>%
  select(Gender, ForeArm.Right, Bicep.Right.Lower, BicepRight, 
         Right.Arm.Surface.Area, Right.Arm.Volume)%>%
  na.omit()%>%
  group_by(Gender)
  
forearm <-arms%>%
  ggplot(aes(x = Gender, y = ForeArm.Right))+
  geom_boxplot()+ 
  stat_summary(fun = mean, geom = "point", shape = 1, size = 3)+
  labs(title = "Forearm",
       y = "Single Forearm Circumference") + 
  theme(plot.title = element_text(hjust = 0.5))
  
biceps <-arms%>%
  select(Gender, Bicep.Right.Lower, BicepRight)%>%
  group_by(Gender)%>%
  summarise(LowerBicep = mean(Bicep.Right.Lower),
            Bicep = mean(BicepRight))%>%
  pivot_longer(!Gender,names_to = "Measurement", values_to = "Mean")%>%
  ggplot(aes(x = Gender, y = Mean, fill = Measurement))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(title = "Bicep Circumferences")+
  theme(plot.title = element_text(hjust = 0.5))

#Torso
torso <- styku_data%>%
  select(Gender, Chest, Torso.Surface.Area, Torso.Volume,Waist..Abdominal., Waist..Lower., Waist..Narrowest.)%>%
  na.omit()%>%
  group_by(Gender)

chest <- torso%>%
  ggplot(aes(x = Gender, y = Chest))+
  geom_boxplot()+ 
  stat_summary(fun = mean, geom = "point", shape = 1, size = 3)+
  labs(title = "Chest",
       y = "Chest Circumference") + 
  theme(plot.title = element_text(hjust = 0.5))
  

waist <-torso%>%
  select(Gender, Waist..Abdominal., Waist..Lower., Waist..Narrowest.)%>%
  group_by(Gender)%>%
  summarise(WaistAbdominal = mean(Waist..Abdominal.),
            WaistLower = mean(Waist..Lower.),
            WaistNarrowest = mean(Waist..Narrowest.))%>%
  pivot_longer(!Gender,names_to = "Measurement", values_to = "Mean")%>%
  ggplot(aes(x = Gender, y = Mean, fill = Measurement))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(title = "Waist Circumferences")+
  theme(plot.title = element_text(hjust = 0.5))

torso.plot <- torso%>%
  select(Gender,Torso.Surface.Area, Torso.Volume)%>%
  group_by(Gender)%>%
  summarise(TorsoSA = mean(Torso.Surface.Area),
            TorsoVolume = mean(Torso.Volume))%>%
  pivot_longer(!Gender,names_to = "Measurement", values_to = "Mean")%>%
  ggplot(aes(x = Gender, y = Mean, fill = Measurement))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(title = "Torso Measurements")+
  theme(plot.title = element_text(hjust = 0.5))

#Proportions  

pro <- styku_data%>%
  select(Gender, Waist.Hip.Ratio..WHO., Bust.Hip.Ratio, Bust.Waist.Ratio)%>%
  na.omit()%>%
  group_by(Gender)


WaistHip <- pro%>%
  ggplot(aes(x = Gender, y = Waist.Hip.Ratio..WHO.))+
  geom_boxplot()+ 
  stat_summary(fun = mean, geom = "point", shape = 1, size = 3)+
  labs(title = "Waist v Hip ",
       y = "Ratio (Waist/Hip)") + 
  theme(plot.title = element_text(hjust = 0.5))

BustHip <- pro%>%
  ggplot(aes(x = Gender, y = Bust.Hip.Ratio))+
  geom_boxplot()+ 
  stat_summary(fun = mean, geom = "point", shape = 1, size = 3)+
  labs(title = "Bust v Hip ",
       y = "Ratio (Bust/Hip)") + 
  theme(plot.title = element_text(hjust = 0.5))

BustWaist <- pro%>%
  ggplot(aes(x = Gender, y = Bust.Waist.Ratio))+
  geom_boxplot()+ 
  stat_summary(fun = mean, geom = "point", shape = 1, size = 3)+
  labs(title = "Bust v Waist",
       y = "Ratio (Bust/Waist)") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_grid(h_w,
          biceps,
          forearm,
          chest,
          torso.plot,
          waist,
          thighs,
          Leg,
          calf,
          WaistHip,
          BustHip,
          BustWaist,
          labels = NULL,
          align = "h")





