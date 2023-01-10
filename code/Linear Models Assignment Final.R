library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

# Question 1a

Teams_small %>% mutate(R = R/G) %>% summarise(tidy(lm(avg_attendance ~ R,data = across())))

Teams_small %>% mutate(HR = HR/G) %>% summarise(tidy(lm(avg_attendance ~ HR,data = across())))

# Question 1b

Teams_small %>% summarise(tidy(lm(avg_attendance ~ W,data = across())))

# Question 1c

Teams_small %>% summarise(tidy(lm(avg_attendance ~ yearID ,data = across())))

# Question 2

d1 <- Teams_small %>% mutate(R = R/G,HR = HR/G) 
cor(d1$R,d1$W)
cor(d1$W,d1$HR)

# Question 3a

Teams_new <- Teams_small %>% mutate(W = round(W/10,0)) %>% filter(W %in% 5:10)
unique(Teams_new$W)

Teams_new %>% filter(W == 8) %>% nrow()

# Question 3b

Teams_new %>% mutate(R = R/G) %>% 
  group_by(W) %>% 
  summarize(tidy(lm(avg_attendance ~ R,data = across()))) %>% filter(term == "R")

Teams_new %>% mutate(HR = HR/G) %>% 
  group_by(W) %>% 
  summarize(tidy(lm(avg_attendance ~ HR,data = across()))) %>% filter(term == "HR")

# Question 4

Teams_new1 <- Teams_small %>% mutate(R = R/G, HR = HR/G)
lm.fit <- lm(avg_attendance ~ R + HR + W + yearID,data = Teams_new1)

# Question 5

predict(lm.fit,data.frame(R = 5,HR = 1.2, W = 80,yearID = 2002))
predict(lm.fit,data.frame(R = 5,HR = 1.2, W = 80,yearID = 1960))

#Question 6

newdata <- Teams %>%
  filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G,
         R = R/G,
         HR = HR/G)
preds <- predict(lm.fit, newdata)
cor(preds, newdata$avg_attendance)
