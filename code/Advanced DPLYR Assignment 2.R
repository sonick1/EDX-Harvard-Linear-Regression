library(tidyverse)
library(HistData)
data("GaltonFamilies")
# set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

# Question 8
galton %>% group_by(pair) %>% summarise(obs = n())

# Question 9
galton %>% group_by(pair) %>% summarise(obs = n(), corr = cor(parentHeight,childHeight))

# Question 10
galton %>% group_by(pair) %>%  summarize(tidy(lm(childHeight ~ parentHeight , data = across()), conf.int = T)) %>%
  filter(term == "parentHeight")

galton %>% group_by(pair) %>%  summarize(tidy(lm(childHeight ~ parentHeight , data = across()), conf.int = T)) %>%
  filter(term == "parentHeight") %>% select(pair,estimate, conf.low, conf.high) %>%  
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

galton %>% group_by(pair) %>%  summarize(tidy(lm(childHeight ~ parentHeight , data = across()), conf.int = T)) %>%
  filter(term == "parentHeight") %>% select(pair,estimate, conf.low, conf.high) %>% mutate(diff =conf.high - conf.low)

galton %>%
  group_by(pair) %>%
  summarize(tidy(lm(childHeight ~ parentHeight, data = across()), conf.int = TRUE)) %>%
  filter(term == "parentHeight" & p.value < .05)
