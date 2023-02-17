library(tidyverse)
library(rstatix)

# set the seed for reproducibility
set.seed(123)

# create simulated data
time <- rep(c(0, 1, 2, 3), each = 30)
sec <- rep(rep(c("small", "medium", "large"), each = 10), times = 4)
value <- c(rnorm(10, mean = 5, sd = 1),
           rnorm(10, mean = 10, sd = 2),
           rnorm(10, mean = 15, sd = 3),
           rnorm(10, mean = 20, sd = 4))
df <- data.frame(time = time, sec = sec, value = value)

# perform t-tests
t_test1 <- df %>% 
  group_by(time) %>% 
  t_test(value ~ sec) %>% 
  add_xy_position(x = "time")

t_test2 <- df %>%
  group_by(sec) %>% 
  t_test(value ~ time) %>% 
  add_xy_position()

# create box plot with p-values
ggplot(df, aes(x = as.factor(time), y = value)) +
  geom_boxplot(aes(fill = sec)) +
  add_pvalue(t_test1, label = "p.adj.signif", xmin = "xmin", xmax = "xmax") +
#  add_pvalue(t_test2, label = "p.adj.signif") +
  theme_bw() +
  labs(x = "Time", y = "Value", fill = "Secretome")
