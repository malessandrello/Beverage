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
  mutate(Value = as_factor(ifelse(Value == 2, 1, 0)),
         bev = as.factor(ifelse(Battery == "Beverage Daily", 1, 0)))

fit <- lme4::glmer(Value ~ day + day*bev + (1|id),
                   data = daily_alc,
                   family = binomial(link = "logit"))

summary(fit)

test_set <- with(daily_alc, tibble(day = c(1:6, 8:13, 15:20)))

predict(fit, newdata = test_set, type = "response", re.form = NA)

fit2<- lme4::glmer(Value ~ bev + (1|id),
                   data = daily_alc,
                   family = binomial(link = "logit"))

test_set2 <- with(daily_alc, tibble(bev = as_factor(c(0,0,0,0,1)))) 

predict(fit2, newdata = test_set2, type = "response", re.form = NA)



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

