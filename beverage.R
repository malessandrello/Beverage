library(tidyverse)
library(readr)
library(rstatix)
library(ggpubr)
library(lme4)
library(lmerTest)

data <- read_csv("Study Data.csv")

data %>% 
  filter(`Question Name` == "daily_alc" & `Participant ID` == 13)
  

data %>% 
  filter(`Question Name` == "week_alc_use" & `Participant ID` == 13)

data %>% 
  filter(str_detect(`Question Name`, "who") & `Participant ID` == 13)


daily_alc <- data %>% 
  drop_na() %>% 
  filter(`Question Name` == "daily_alc") %>% 
  group_by(`Participant ID`, Battery) %>%
  summarise(Value = mean(Value))

daily_alc %>% 
  ggplot(aes(Battery, Value))+
  geom_boxplot()+
  geom_pwc(method = "t_test")


daily_alc_w <- daily_alc %>% 
  pivot_wider(names_from = Battery, values_from = Value)

t.test(daily_alc_w$`Beverage Daily`, daily_alc_w$`Pre Beverage Daily`)



mean(is.na(data))


who <- data %>% 
  drop_na() %>% 
  filter(str_detect(`Question Name`, "who")) %>% 
  group_by(`Participant ID`, `Study Day`) %>% 
  summarise(Value = sum(Value)*4)




         
