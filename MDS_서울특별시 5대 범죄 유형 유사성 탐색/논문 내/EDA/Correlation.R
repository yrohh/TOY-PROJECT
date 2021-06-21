setwd("C:\\Users\\yoonjun\\Desktop\\jun\\4_1\\machine_learning\\논문\\논문 작성")
data <- read.csv('data.csv')
data2 <- read.csv('data_인구2.csv')

library(dplyr)

head(data)
str(data)

data[1,6]/sum(data[1,3:7])
data <- data %>% mutate(절도비율 = 절도/(살인+강도+강간강제추행+절도+폭력))
data <- data[c(1,2,8)]
data
head(data2)
colnames(data)
colnames(data2) <- c('기간', '자치구', '청년비율')
data <- merge(data, data2, by=c('기간','자치구'))

cor.test(data$청년비율, data$절도비율)

re <- lm(data = data, 절도비율 ~ 청년비율)
summary(re)

plot(data$청년비율, data$절도비율) + abline(re) + title('20-40세 인구 비율과 절도 범죄 비율 상관관계')


