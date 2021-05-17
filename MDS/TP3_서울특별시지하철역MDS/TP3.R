setwd('')
getwd()
data <- read.csv("data.csv")
str(data) # 606개역, 24시간.
head(data)

# 매트릭스 생성.
data.matrix <- cbind(data[2:49])
data.matrix
data.matrix <- as.matrix(data.matrix)
rownames(data.matrix) <- data[[1]] # 매트릭스 인덱스명 설정.
View(data.matrix)
str(data.matrix)
# data.matrix[,2:48] <- as.numeric(data.matrix[,2:48]) # 연산이 가능하도록 값들을 numeric으로 변경.

# 행렬곱.
data.mul <- data.matrix %*% t(data.matrix)
data.mul

# 거리 연산.
data.dist <- dist(data.mul)
data.dist

# mds 적용_좌표 도출.
data.mds <- cmdscale(data.dist)
data.mds

# 좌표 시각화.
plot(data.mds, type='n',
     main='그래프 제목')
text(data.mds, rownames(data.mds),
     col='maroon', cex=0.7)