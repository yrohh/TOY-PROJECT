setwd('C:\\Users\\yoonjun\\Desktop\\TP3')
getwd()
data <- read.csv("data.csv")
str(data) # 606개역, 24시간.
head(data)

data.matrix <- cbind(data[2:49])
data.matrix
data.matrix <- as.matrix(data.matrix)
rownames(data.matrix) <- data[[1]]
View(data.matrix)
str(data.matrix)
data.matrix[,2:48] <- as.numeric(data.matrix[,2:48])

data.mul <- data.matrix %*% t(data.matrix)
data.mul
data.dist <- dist(data.mul)
data.dist
data.mds <- cmdscale(data.dist)
data.mds

plot(data.mds, type='n',
     main='MDS_서울특별시 지하철역 간 유사도-시간대별 승하차 인원수를 중심으로')
text(data.mds, rownames(data.mds),
     col='maroon', cex=0.7)
library(ggplot2)
ggsave('mds.png', dpi=300)
