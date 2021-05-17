setwd('C:\\Users\\yoonjun\\Desktop\\jun\\4_1\\machine_learning\\논문\\TP5_전국지역MDS(형법범)')
getwd()
data <- read.csv("data.csv")
str(data) # 17개 지역, 48개 형법범종.
head(data)

data.matrix <- cbind(data[2:48])
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
     main='MDS_전국 지역 간 유사도-형법범 수를 중심으로')
text(data.mds, rownames(data.mds),
     col='black', cex=0.7)
library(ggplot2)
ggsave('mds.png', dpi=300)
