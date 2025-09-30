library(tidyverse)
library(readr)
library(rstatix)
library(ggpubr)
library(lme4)
library(lmerTest)
library(logitr)

data <- read_csv("Study Data.csv") %>% 
  rename(id = `Participant ID`, day = `Study Day`)



daily_alc <- data %>% 
  drop_na() %>% 
  filter(`Question Name` == "daily_alc") %>% 
  mutate(Value = ifelse(Value == 2, 1, 0)) %>% 
group_by(id, Battery) %>%
  summarise(Value = mean(Value)) 

daily_alc <- data %>% 
  drop_na() %>% 
  filter(`Question Name` == "daily_alc") %>% 
  mutate(Value = ifelse(Value == 2, 1, 0))


daily_alc %>% 
  ggplot(aes(Battery, Value))+
  geom_boxplot()+
  geom_pwc(method = "t_test")


daily_alc_w <- daily_alc %>% 
  pivot_wider(names_from = Battery, values_from = Value)

t.test(daily_alc_w$`Beverage Daily`, daily_alc_w$`Pre Beverage Daily`)

model <- lmerTest::lmer(Value ~ Battery + (1|id),
                    na.action = na.omit,
                    REML = TRUE,
                    data = daily_alc)

model2_pre <- lme4::glmer(Value ~ day + (1|id),
                     na.action = na.omit,
                     family = binomial(link = "logit"),
                     data = daily_alc)

summary(model2_pre)


model2 <- lme4::glmer(Value ~ day + (1|id),
                      na.action = na.omit,
                      family = binomial(link = "logit"),
                      data = daily_alc_bev)


summary(model2)



who <- data %>% 
  drop_na() %>% 
  filter(str_detect(`Question Name`, "who")) %>% 
  group_by(id, day, Battery) %>% 
  summarise(Value = sum(Value)*4)

a <- data %>%
  filter(`Question Name` == "daily_alc") %>% 
  drop_na() %>% 
  group_by(Battery) %>% 
  get_summary_stats(Value)

get_summary_stats(data = a, Value)


         
daily_alc_pre <- data %>% 
  drop_na() %>% 
  filter(`Question Name` == "daily_alc" & Battery == "Pre Beverage Daily") %>% 
  mutate(Value = ifelse(Value == 2, 1, 0))
         

daily_alc_bev <- data %>% 
  drop_na() %>% 
  filter(`Question Name` == "daily_alc" & Battery == "Beverage Daily") %>% 
  mutate(Value = ifelse(Value == 2, 1, 0))


x <- predict(model, type = "response")[1:21]

f <- tibble(day = c(1:21), x)