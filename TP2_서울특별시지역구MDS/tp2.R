setwd('C:\\Users\\yoonjun\\Desktop\\TP2')
getwd()
data <- read.csv("data.csv")
str(data) # 25개 지역, 7개 특성.
head(data)
data.matrix <- cbind(data[2],data[3],data[4],data[5],data[6],data[7])
data.matrix <- as.matrix(data.matrix)
rownames(data.matrix) <- data[[1]]
data.matrix

data.mul <- data.matrix %*% t(data.matrix)
data.mul
data.dist <- dist(data.mul)
data.dist
data.mds <- cmdscale(data.dist)
data.mds

plot(data.mds, type='n',
     main='MDS_서울특별시 구 간 유사도-주변 문화시설 수를 중심으로')
text(data.mds, rownames(data.mds),
     col='maroon', cex=0.7)
library(ggplot2)
ggsave('mds.png', dpi=300)
