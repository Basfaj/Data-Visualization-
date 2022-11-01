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
##Frequency and Percentage table
PC3 <- titanic %>%
  group_by(Sex, Pclass) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(freq))
PC3 


