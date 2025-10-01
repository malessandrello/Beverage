library(tidyverse)
library(readr)
library(rstatix)
library(ggpubr)
library(lme4)
library(lmerTest)
library(logitr)

data <- read_csv("Study Data.csv") %>% 
  rename(id = `Participant ID`,
         day = `Study Day`,
         question = `Question Name`)

data_dup <- data %>% 
  filter(str_detect(question, "who_5$")) %>% 
  select(id, day, Battery, question) %>% 
  drop_na()


  