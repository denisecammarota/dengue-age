library(tidyverse)

setwd('C:/Users/denis/Documents/dengue-age')

file_list <- list.files('data', pattern = "\\.csv$", full.names = TRUE)
df <- data.frame()

for(file in file_list){
  print(substr(file,10,13))
  a <- read.csv2(file, sep = ';', fileEncoding = "latin1")
  a['Ano'] <- substr(file,10,13)
  df <- rbind(df, a)
}

df <- df %>% mutate(Mun = substr(Municipio,1,6))
colnames(df)<- c('Municipio','00 a 04', '05 a 09', '10 a 14',
                 '15 a 19', '20 a 29', '30 a 39', '40 a 49',
                 '50 a 59', '60 a 69', '70 a 79', '80 e +', 
                 'Total', 'Ano', 'Mun')
df <- df %>% select('Ano', 'Mun', '00 a 04', '05 a 09', '10 a 14',
                    '15 a 19', '20 a 29', '30 a 39', '40 a 49',
                    '50 a 59', '60 a 69', '70 a 79', '80 e +')
rm(a)
save(df, file = 'pop_muns.RData')





