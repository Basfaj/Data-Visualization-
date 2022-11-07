# Firstly setting of the working directory by using function "setwd" or 
# manually by running code "file.choose()"
setwd("C:/Users/USER/Desktop/FNABHABAF/DATA science/R/R_Praticals")
getwd()

#installing and loading of packages needed
install.packages("readr") #to read data file in csv format
install.packages("readxl") #to read data file in xlxs format
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2") #to plotting graph
installed.packages("formattable") #to set output to table format

#load package
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(formattable)

#load data 
#since I have set my working directory I can just call the name of the file
OONP <-read.csv("OONPFarmers.csv")
head(OONP)

##data waggling

#data EDA
library(psych) #this library contain function called "describe" use to the full
               # description of the data
describe(OONP)

##Descriptive analysis of some socio-economic characteristic via frequency table
### socio-economic characteristic 1
Range <- OONP %>%
  group_by(Range) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(freq))
Range

### socio-economic characteristic 2
Age <- OONP %>%
  group_by(Age_Range) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(freq))
Age

### socio-economic characteristic 3
Education_level <- OONP %>%
  group_by(Edulevel) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(freq))
Education_level

### socio-economic characteristic 4
Farming_type <- OONP %>%
  group_by(Tfarming) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(freq))
Farming_type

### socio-economic characteristic 5
Yearly_Income <- OONP %>%
  group_by(Icrang) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(freq))
Yearly_Income

### socio-economic characteristic 6
Marital_status <- OONP %>%
  group_by(Mstatus) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(freq))
Marital_status

####
HouseSize <- OONP %>%
  group_by(HouseSize, range(1)) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(freq))
HouseSize
#####

#Data visualization USING ggplot

## Bar Chart
Age_BarChart <- ggplot(OONP, aes(x= Age_Range, fill = Age_Range)) + 
  geom_bar(aes(y =..count../12)) +
  labs(y= "Age Level") #"coord_flip()" #add flipping feature
Age_BarChart

Edulevel_BarChart <- ggplot(OONP, aes(x= Edulevel, fill = Edulevel)) + 
  geom_bar(aes(y =..count../12)) +
  labs(y= "Age Level")  #"coord_flip()" #add flipping feature
Edulevel_BarChart

##Using facet wrap to create separate bar chart of different variable

BRankF <- ggplot(OONP, aes(x=Tfarming, fill=Tfarming))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = -55, hjust = 0, vjust = 1))+
  theme(legend.position = "none")+
  facet_wrap("Prank")
BRankF

BRankEDu <- ggplot(OONP, aes(x=Edulevel, fill=Edulevel))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = -55, hjust = 0, vjust = 1))+
  theme(legend.position = "none")+
  facet_wrap("Prank")
BRankEDu
##other method of plotting barchart for numeric variable
barplot (sort (OONP$HouseSize))

##Density_Graph
AgeGraph <- ggplot(OONP, aes(x=Age)) +
  geom_density()  
AgeGraph

village <- ggplot(data=OONP, aes(x=Village, fill=Village)) + 
  geom_density(alpha = 0.3)
village

##Histogram plot
Income1 <- ggplot(data=OONP, aes(x=Income, fill=Income)) + 
  geom_histogram(bins = 5)  
Income1

Age_2 <-ggplot(OONP, aes(x=Age)) + 
  geom_histogram(color="black", fill="white")
Age_2
###other method of histogram plot

hist (OONP$Income)

# Histogram with density plot
HouseSize_1 <- ggplot(data=OONP, aes(x=HouseSize)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.3, fill="#FF6666") 
HouseSize_1

##BOXPLOT
farmingBP <- ggplot(data=OONP, aes(x=Tfarming, y= Pscores, fill=Tfarming)) + 
  geom_boxplot()
farmingBP

educationBP <- ggplot(data=OONP, aes(x=Edulevel, y= Pscores, fill=Edulevel)) + 
  geom_boxplot()
educationBP
### other method of plotting Boxplot
boxplot (OONP$Age)

##Create a heatmap
ggplot(OONP, aes(x=Tfarming, y=Edulevel, fill=n)) +
  geom_tile()




##Scatter Plot
age_Pscores <- ggplot(data=OONP, aes(x=Age, y=Pscores, colour=Age)) + 
  geom_point()+
  geom_smooth(method = lm)
age_Pscores

HS_Pscores <- ggplot(data=OONP, aes(x=HouseSize, y=Pscores, colour=HouseSize)) + 
  geom_point()+
  geom_smooth(method = lm)
HS_Pscores

HS_Pscores1 <- ggplot (OONP, aes (x=HouseSize, y=Pscores)) +
  geom_point()+
  geom_smooth(method=loess)
HS_Pscores1  

#Add titles and text, change text size and color of text Scatterplot 
age_Pscores1 <- ggplot(data=OONP, aes(x=Age, y=Pscores, colour=Age)) + 
  geom_point()+
  geom_smooth(method = lm)+
  annotate("Regression", x=70, y=10, label="impact of age on pscore",color="green") +
  labs(title="REGRESSION PLOT")+
  labs(subtitle = "reaction of independent(Y) on dependent(X) varible")+
  theme(plot.subtitle = element_text(size=6))
age_Pscores1


# data visualization with "lessR" package
#"lessR" package is more sophisticated than other visualization packages in R
# it is unique by it short code command and multiple output characters
install.packages("lessR")
library(lessR)

## data visualizations with lessR package using discrete or categorical variables

BarChart(Mstatus, data=OONP)

BarChart(Mstatus, data=OONP, by1 = Tfarming)

BarChart(Mstatus, data=OONP, by = Tfarming)

BarChart(Mstatus, data=OONP, by1 = Prank)

PieChart(Edulevel, hole=0, data=OONP) #full circle pie chart

PieChart(Edulevel, data=OONP)  # Doughnut chart 

##data visualizations with lessR package using continuous, ratio or interval variables

hist(OONP$Age)




            