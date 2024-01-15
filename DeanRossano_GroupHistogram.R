library(tidyverse)
library(dplyr)
library(colorspace)
##Load Dataset
df = read.csv("Sampledata2.csv")

##Break data into 3 groups of Crime Rate Ranges
df = df %>% mutate(RangeGroup = case_when(CrimeRate < 250 ~ "CrimeRate < 250",
                                          CrimeRate >= 250 & CrimeRate <=500 ~ "250 <= CrimeRate <= 500",
                                          CrimeRate > 500 ~ "CrimeRate < 500"))

##Create color vectors
n = length(unique(df$Year))
year_col = diverge_hcl(n)
range_col = c("green", "blue", "red")

## Plot Graph with Year as Color Vector
ggplot(df, aes(x = CrimeRate, color=as.factor(Year), fill = as.factor(Year))) +
  geom_histogram() +
  scale_color_manual(values = year_col)+
  scale_fill_manual(values = year_col)+
  labs(title= "Crime Rate by Year", color= "Year", fill="Year") + ylab("Count") +
  xlab("Crime Rate")
  

## Plot Graph with RangeGroup as Color Vector
ggplot(df, aes(x = CrimeRate, color=as.factor(RangeGroup), fill = as.factor(RangeGroup))) +
  geom_histogram() +
  scale_color_manual(values = range_col)+
  scale_fill_manual(values = range_col)+
  labs(tite= "Crime Rate by Range", color= "Range", fill="Range") + ylab("Count") +
  xlab("Crime Rate")
