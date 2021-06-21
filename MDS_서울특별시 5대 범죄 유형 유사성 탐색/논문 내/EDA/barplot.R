library(dplyr)
library(ggplot2)
library(plotly)

setwd("C:\\Users\\yoonjun\\Desktop\\jun\\4_1\\machine_learning\\논문\\논문 작성")
data <- read.csv('data.csv')

head(data)
str(data)

data_중구 <- data %>% filter(자치구 == '중구')
data_중구 <- data_중구[,3:7]

중구m <- as.matrix(data_중구)
중구m <- t(중구m)
colnames(중구m) <- c('2014','2015','2016','2017','2018','2019')
중구m

중구bp <- barplot(중구m, main="중구, 2014-2019 범죄 유형별 발생 빈도 분포", beside=T, col=rainbow(nrow(중구m)), ylim=c(0,350))
legend("topright", c('살인','강도', '강간강제추행', '절도', '폭력'), box.lty=0, border="white", cex=1, fill=rainbow(nrow(중구m)))

###

data_종로구 <- data %>% filter(자치구 == '종로구')
data_종로구 <- data_종로구[,3:7]

종로구m <- as.matrix(data_종로구)
종로구m <- t(종로구m)
colnames(종로구m) <- c('2014','2015','2016','2017','2018','2019')
종로구m

종로구bp <- barplot(종로구m, main="종로구, 2014-2019 범죄 유형별 발생 빈도 분포", beside=T, col=rainbow(nrow(종로구m)), ylim=c(0,300))
legend("topright", c('살인','강도', '강간강제추행', '절도', '폭력'), box.lty=0, border="white", cex=1, fill=rainbow(nrow(종로구m)))
