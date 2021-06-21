setwd("C:\\Users\\yoonjun\\Desktop\\jun\\4_1\\machine_learning\\논문\\논문 작성\\논문 첨부\\MDS 예시")
data <- read.csv('data.csv')

head(data)
str(data)
summary(data)

matrix <- cbind(data[2], data[3], data[4], data[5], data[6], data[7], data[8])
matrix <- as.matrix(matrix)
matrix
rownames(matrix) <- data[[1]]

dist <- dist(matrix)
dist
mds <- cmdscale(dist)
mds
# 그래프 시각화.
# install.packages("extrafont")
library(extrafont)
fonts()
fonttable()
# font_import(pattern = "NanumGothic")

# MDS 시각화 함수.

plot(mds, type='n') + title(main='학생별 색깔 선호도 유형 MDS', cex.main=2, family = "NanumGothic") +
  text(mds, rownames(mds), cex=2)

