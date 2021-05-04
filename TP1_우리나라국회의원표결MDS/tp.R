setwd("디렉토리")
raw <- read.csv("국회의원본회의표결정보.csv")
head(raw)
str(raw)
summary(raw)

library(reshape)
raw$의안번호 <- as.factor(raw$의안번호)
unique(raw$정당)
# 국민의힘 ~ 국민의당 => 1 ~ 8
raw$정당 <- ifelse(raw$정당=='국민의힘',1,
                 ifelse(raw$정당=='더불어민주당',2,
                        ifelse(raw$정당=='무소속',3,
                               ifelse(raw$정당=='기본소득당',4,
                                      ifelse(raw$정당=='정의당',5,
                                             ifelse(raw$정당=='시대전환',6,
                                                    ifelse(raw$정당=='열린민주당',7,8)))))))
raw$정당 <- as.factor(raw$정당)
# 표결결과 찬성 1, 반대 -1, 기권 0
raw$표결결과 <- ifelse(raw$표결결과=='찬성',1,
                   ifelse(raw$표결결과=='반대',-1,0))
raw$표결결과 <- as.numeric(raw$표결결과)

# 의원-의안 표결결과 행렬 생성.
data.matrix <- cast(raw, 의원 ~ 의안번호, value = '표결결과' ) 
View(data.matrix)
str(data.matrix)
dim(data.matrix) # 272명의 의원과, 15개의 의안
data.matrix <- as.matrix(data.matrix)
data.matrix

# 의원-의안 행렬곱 
mp.data.matrix <- data.matrix %*% t(data.matrix)

# 거리 도출
mp.data.matrix.dist <- dist(mp.data.matrix)

# 좌표 추출
mp.data.matrix.dist.mds <- cmdscale((mp.data.matrix.dist))
mp.data.matrix.dist.mds

# 시각화 위한 데이터프레임 생성
rollcall.mds <- as.data.frame(mp.data.matrix.dist.mds)
의원_<- row.names(rollcall.mds)
for_party <- raw[,c('name','party')]
for_party <- unique(for_party)
for_party <- for_party[c(order(for_party$name)),]
View(for_party)

rollcall.mds <- data.frame(x=rollcall.mds$V1,
                           y=rollcall.mds$V2,
                           name=의원_,
                           party=for_party$party)
head(rollcall.mds)

base <- ggplot(rollcall.mds, aes(x = x, y = y)) +
  scale_size(range = c(2,2), guide = 'none') +
  scale_alpha(guide = 'none') +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggtitle("Roll Call Vote MDS Clustering_KOR") +
  xlab("") +
  ylab("")

print(base + geom_point(aes(shape = party,
                                alpha = 0.75,
                                size = 2)))

      
