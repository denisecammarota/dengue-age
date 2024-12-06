library(tidyverse)
library(ggplot2)

df_fit <- df_mean
df_fit <- df_fit %>% select(age_range, rate)
df_fit <- df_fit %>% mutate(n = seq(1,11,1))
df_fit <- df_fit %>% select(n, rate)
df_fit

model <- nls(rate ~ a * (n/d)^(b-1) * (1-(n/d))^(c-1), data = df_fit, start = list(a = 1.0, b = 3.0, c = 4.0, d = 11))
summary(model)

x <- seq(1,11,1)
y <- 6.013e-07 * (x/1.191e+01)^(2.312e+00 - 1) * (1 - (x/1.191e+01))^(2.192e+00 - 1)
plot(x,y)
