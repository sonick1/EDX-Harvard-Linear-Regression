set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
library(tidyverse)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# Question 8

mu_mother <- mean(female_heights$mother)
sd_mother <- sd(female_heights$mother)

mu_daughter <- mean(female_heights$daughter)
sd_daughter <- sd(female_heights$daughter)

cor_coeff <- cor(female_heights$mother,female_heights$daughter)

# Question 9

slope_dm <- cor_coeff*(sd_daughter/sd_mother)
b_dm <- mu_daughter - slope_dm*mu_mother 
cor_coeff * sd_daughter/sd_mother

# Question 10
(cor_coeff^2)*100


# Question 11
daughter_height <- slope_dm*60 + b_dm
