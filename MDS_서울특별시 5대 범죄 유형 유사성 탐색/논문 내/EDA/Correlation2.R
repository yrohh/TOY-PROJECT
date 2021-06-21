setwd("C:\\Users\\yoonjun\\Desktop\\jun\\4_1\\machine_learning\\논문\\논문 작성")
cctv <- read.csv('data_cctv.csv')
data <- read.csv('data_범죄비.csv')

library(dplyr)

head(cctv)
head(data)

data <- data[c(1,2,8)]
head(data)
head(cctv)
all_df <- merge(cctv, data, by=c('기간','자치구'))
head(all_df)
View(all_df)

cor(all_df$증가비, all_df$범죄증가비)


