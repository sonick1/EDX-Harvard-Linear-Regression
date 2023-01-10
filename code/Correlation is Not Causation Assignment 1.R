# Question 1
library(tidyverse)
library(dslabs)
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each=N), 
                   x = rnorm(N * g), 
                   y = rnorm(N * g))
  
res <- sim_data |> 
  group_by(group) |> 
  summarize(r = cor(x, y)) |> 
  arrange(desc(r))
res

sim_data |> filter(group == res$group[which.max(res$r)]) |>
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_smooth(method = "lm")


# Quesion 5
?admissions
