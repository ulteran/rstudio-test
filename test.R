library(ggplot2)
library(ggpubr)
library(rstatix)
library(dplyr)
library(ggprism)

# set the seed for reproducibility
set.seed(123)

# create a vector of the time groups
time <- rep(c(0, 1, 2, 3), each = 30)

# create a vector of the secret groups
sec <- rep(rep(c("small", "medium", "large"), each = 10), times = 4)

# create a vector of observations for each group
value <- c(rnorm(10, mean = 5, sd = 1),
                  rnorm(10, mean = 10, sd = 2),
                  rnorm(10, mean = 15, sd = 3),
                  rnorm(10, mean = 20, sd = 4))

# create the data frame
df <- data.frame(time = time,
                 sec = sec,
                 value = value)

# print the data frame
df
# 

df_stat <- df %>% 
  rstatix::group_by(
    time
  ) %>% 
  t_test(
    value ~ sec
  ) %>% 
  rstatix::add_x_position(x = "time", dodge = 0.75) %>% 
  rstatix::add_y_position(fun = "max", step.increase = 0.05)

df_stat_2 <- df %>%
  t_test(
    value ~ time
  ) %>% 
  rstatix::add_x_position(x = "time", dodge = 0.75) %>% 
  rstatix::add_y_position(fun = "max", step.increase = 0.2) %>% 
  mutate(y.position = y.position + 4)

ggplot(df, aes(x = as.factor(time), y = value)) +
  geom_boxplot(aes(fill = sec)) +
  add_pvalue(
    df_stat,
    label = "p.adj.signif",
    xmin = "xmin", 
    xmax = "xmax",
    tip.length = 0.005
  ) +
  ggprism::add_pvalue(
    df_stat_2,
    label = "p.adj.signif",
    xmin = "xmin", 
    xmax = "xmax",
    tip.length = 0.005
  )
