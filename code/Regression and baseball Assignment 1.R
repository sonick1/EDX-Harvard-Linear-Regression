# Question 3
bb <- 2
s <- 4
d <- 1
t <- 0
h <- 1
score <- 0.371*bb + 0.519*s + 0.771*d + 1.24*t + 1.44*h
score

bb <- 1
s <- 6
d <- 2
t <- 1
h <- 0
  score <- 0.371*bb + 0.519*s + 0.771*d + 1.24*t + 1.44*h
score

# Question 9a
library(Lahman)
library(tidyverse)
library(broom)

Teams %>% filter(yearID %in% 1971) %>% lm(R ~ BB + HR,data = .) %>% tidy()

# Question 10

res <- Teams %>% filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>% 
  summarize(tidy(lm(R ~ BB + HR,data = across()))) %>% ungroup()
head(Teams)

res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

# Question 11
res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") 
