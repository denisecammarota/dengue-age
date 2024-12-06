library(tidyverse)
library(ggplot2)

setwd('C:/Users/denis/Documents/dengue-age')

# Loading data of age classes for municipality and year #################################

load('pop_muns.RData')
df_pop <- df
df_pop <- df_pop %>% filter(Ano <= 2020) %>% filter(Ano >= 2007)
rm(df)

# Loading data on age classes and cases by year #########################################

load('age_classes.RData')
df_cases <- df_cases %>% filter(ANO <= 2020) %>% filter(ANO >= 2007)
df_cases <- df_cases %>% 
  mutate(age_range_new = case_when(
    NU_IDADE_N == '0.0' ~ '00 a 04',
    age_range == '(19, 29]' ~ '20 a 29',
    age_range == '(59, 69]' ~ '60 a 69',
    age_range == '(9, 14]' ~ '10 a 14',
    age_range == '(69, 79]' ~ '70 a 79',
    age_range == '(39, 49]' ~ '40 a 49',
    age_range == '(49, 59]' ~ '50 a 59',
    age_range == '(0, 4]' ~ '00 a 04',
    age_range == '(29, 39]' ~ '30 a 39',
    age_range == '(4, 9]' ~ '05 a 09',
    age_range == '(14, 19]' ~ '15 a 19',
    age_range == '(79, 120]' ~ '80 e +'
  )) %>% filter(!is.na(age_range_new))
df_cases <- df_cases %>% group_by(ANO, ID_MN_RESI, age_range_new) %>% summarise(n_cases = n())
df_cases <- df_cases %>% ungroup()
df_cases <- df_cases %>% complete(ANO, ID_MN_RESI, age_range_new)
df_cases[is.na(df_cases)] <- 0

# Calculating for a municipality the age pattern ##########################################
mun <- 355030

df_cases <- df_cases %>% filter(ID_MN_RESI == mun)
df_pop <- df_pop %>% filter(Mun == mun)
df_cases <- df_cases %>% pivot_wider(names_from = age_range_new, values_from = n_cases)
df_cases <- df_cases %>% select(!c(ANO, ID_MN_RESI)) # matrix of cases
df_pop <- df_pop %>% select(!c(Ano, Mun)) # matrix of population

df_tmp <- data.frame(rowSums(df_cases))
df_cases_year <- data.frame(replicate(11, df_tmp$rowSums.df_cases.))
colnames(df_cases_year) <- colnames(df_cases)

# turning everything into numeric
df_cases <- data.frame(lapply(df_cases, as.numeric))
df_pop <- data.frame(lapply(df_pop, as.numeric))
df_cases_year <- data.frame(lapply(df_cases_year, as.numeric))
  
# dividing to get age pattern
df_final <- df_cases
df_final <- df_final/df_pop
df_final <- df_final/df_cases_year

# plotting the pattern
colnames(df_final) <- colnames(df_cases)
df_final['Ano'] <- seq(2007,2020,1)
df_final <- df_final %>% pivot_longer(!Ano, names_to = 'age_range', values_to = 'rate')

p <- ggplot(df_final, aes(x = age_range, y = rate, color = factor(Ano), group = Ano)) + 
  geom_point() + 
  geom_line() + 
  labs(
    x = "Age Range",
    y = "Rate",
    color = "Year"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# doing the mean and standar deviation
df_mean <- df_final %>% group_by(age_range) %>% summarise(rate = mean(rate)) 
df_mean <- df_mean %>% ungroup()
df_mean['Ano'] <- 'Mean'
df_mean <- df_mean %>% select(Ano, age_range, rate)
ggplot(df_mean, aes(x = age_range, y = rate)) + geom_point(group = 1) + geom_line(group = 1)

# plotting everything together
a <- rbind(df_final, df_mean)
p <- ggplot(a, aes(x = age_range, y = rate, color = factor(Ano), group = Ano)) + 
  geom_point() + 
  geom_line() + 
  labs(
    x = "Age Range",
    y = "Rate",
    color = "Year"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
