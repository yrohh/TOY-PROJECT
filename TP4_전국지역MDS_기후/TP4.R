setwd('C:\\Users\\yoonjun\\Desktop\\jun\\4_1\\machine_learning\\논문\\TP4')
data <- read.csv("data.csv")

str(data)
data <- data[-c(1,3,10)] # 지점, 일시 제외, 결측치 많은 합계.일사량 제외.
summary(data) # 평균기온, 상대습도, 풍속에 NA 존재.
# 결측치 -> 평균치 대체.
is.na(data[,2],2)
mean(data[,2], na.rm = T)
mean(data[,6], na.rm = T)
mean(data[,8], na.rm = T)
for(i in c(2,6,8)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

data.matrix <- cbind(data[2:8])
data.matrix
data.matrix <- as.matrix(data.matrix)
rownames(data.matrix) <- data[[1]]
View(data.matrix)
str(data.matrix)

data.mul <- data.matrix %*% t(data.matrix)
data.mul
data.dist <- dist(data.mul)
data.dist
data.mds <- cmdscale(data.dist)
data.mds

plot(data.mds, type='n',
     main='MDS_전국 지역 간 유사도-지역별 평년 기후를 중심으로')
text(data.mds, rownames(data.mds),
     col='maroon', cex=0.7)
library(ggplot2)
ggsave('mds.png', dpi=300)
