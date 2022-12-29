library(Lahman)
library(tidyverse)
df <- Teams %>% filter(yearID %in% 1961:2001)

# correlation btw number of runs and at bats
cor(df$R/df$G,df$AB/df$G)

# correlation btw win rate vs error rate
cor(df$W/df$G,df$E/df$G)

# correlation btw X3B and X2B
cor(df$X3B/df$G,df$X2B/df$G)
