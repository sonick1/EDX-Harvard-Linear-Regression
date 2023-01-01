# compute RSS for any pair of beta0 and beta1 in Galton's data
library(HistData)
library(tidyverse)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# plot RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

# Question 1 B0 = 36
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

# Question 3
library(Lahman)
df <- Teams %>% filter(yearID %in% 1961:2001) %>% mutate(BB_G = BB/G, R_G = R/G, HR_G = HR/G)
lm.runs <- lm(formula = R_G ~ BB_G + HR_G, data = df) 
summary(lm.runs)

# Question 4
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
hist(lse$beta_0)
hist(lse$beta_1)

# Question 5

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)


# Question 7
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

lm.mother <- lm(formula = mother ~ daughter,data = female_heights)
summary(lm.mother)

# Question 8 
female_heights$predicted_mother <- predict(lm.mother)
head(female_heights)

# Question 9
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb) %>% 
  group_by(playerID) %>% 
  summarize(avg_s = mean(singles), avg_bb = mean(bb))

bat_99_01 %>% 
  filter(avg_s  > 0.2) %>% 
  nrow()

bat_99_01 %>% 
  filter(avg_bb  > 0.2) %>% 
  nrow()

# Question 10
b12 <- inner_join(bat_99_01,bat_02,by = "playerID")
cor(b12$singles,b12$avg_s)
cor(b12$bb,b12$avg_bb)

# Question 11
par(mfrow = c(1,2))
plot(b12$singles,b12$avg_s)
plot(b12$bb,b12$avg_bb)

# Question 12
lm.singles <- lm(formula = singles ~ avg_s,data = b12)
summary(lm.singles)

lm.bb <- lm(formula = bb ~ avg_bb,data = b12)
summary(lm.bb)
