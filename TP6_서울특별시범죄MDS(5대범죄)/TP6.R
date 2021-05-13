setwd("C:\\Users\\yoonjun\\Desktop\\jun\\4_1\\machine_learning\\논문\\TP6_서울특별시범죄MDS(5대범죄)")
data <- read.csv('data.csv')

library(dplyr)
head(data)
str(data)
summary(data)
# 수치값 속한 피처 타입 numeric 변경.
data$살인 <- as.numeric(data$살인)
data$강도 <- as.numeric(data$강도)
data$강간강제추행 <- as.numeric(data$강간강제추행)
data$절도 <- as.numeric(data$절도)
data$폭력 <- as.numeric(data$폭력)

data.2015 <- data %>% filter(기간 == "2015")
data.2016 <- data %>% filter(기간 == "2016")
data.2017 <- data %>% filter(기간 == "2017")
data.2018 <- data %>% filter(기간 == "2018")
data.2019 <- data %>% filter(기간 == "2019")

# MinMax 정규화 함수 생성.
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

# 5개 범죄 수 MinMax 정규화.
data.2015[3:7] <- normalize(data.2015[3:7])
data.2016[3:7] <- normalize(data.2016[3:7])
data.2017[3:7] <- normalize(data.2017[3:7])
data.2018[3:7] <- normalize(data.2018[3:7])
data.2019[3:7] <- normalize(data.2019[3:7])

# 매트릭스 생성 함수.
get_matrix <- function(df) {
  ma <- cbind(df[3:7])
  ma <- as.matrix(ma)
  rownames(ma) <- df[[2]]
  return(ma)
}

# 매트릭스 생성.
ma.2015 <- get_matrix(data.2015)
ma.2016 <- get_matrix(data.2016)
ma.2017 <- get_matrix(data.2017)
ma.2018 <- get_matrix(data.2018)
ma.2019 <- get_matrix(data.2019)

# mds 도출 함수.
get_mds <- function(ma) {
  mul <- ma %*% t(ma)
  dist <- dist(mul)
  mds <- cmdscale(dist)
  return(mds)
}

# 연별 mds 도출.
mds.2015 <- get_mds(ma.2015)
mds.2016 <- get_mds(ma.2016)
mds.2017 <- get_mds(ma.2017)
mds.2018 <- get_mds(ma.2018)
mds.2019 <- get_mds(ma.2019)
mds.2019[,1] <- -(mds.2019[,1]) # 다른 해와 x축이 반대.
# 그래프 시각화.
# install.packages("extrafont")
library(extrafont)
# fonts()
# fonttable()
# font_import(pattern = "NanumGothic")

# MDS 시각화 함수.
make_plot <- function(mds,n) {
  plot(mds, type='n', ann=F, axes=F) +
    title(main=paste(n, "년 지역별 유사도 5대 범죄", sep=""), cex.main=2, family = "NanumGothic") +
    text(mds, rownames(mds), col='black', cex=1.3)
}
# 연도별 MDS 시각화.
make_plot(mds.2015, 2015)
make_plot(mds.2016, 2016)
make_plot(mds.2017, 2017)
make_plot(mds.2018, 2018)
make_plot(mds.2019, 2019)

# 5년 데이터 프레임 생성.
mds.2015 <- cbind(mds.2015, year=2015)
mds.2016 <- cbind(mds.2016, year=2016)
mds.2017 <- cbind(mds.2017, year=2017)
mds.2018 <- cbind(mds.2018, year=2018)
mds.2019 <- cbind(mds.2019, year=2019)
mds.2015 <- as.data.frame(mds.2015)
mds.2016 <- as.data.frame(mds.2016)
mds.2017 <- as.data.frame(mds.2017)
mds.2018 <- as.data.frame(mds.2018)
mds.2019 <- as.data.frame(mds.2019)
location <- rownames(mds.2015)
mds.2015$location <- location
mds.2016$location <- location
mds.2017$location <- location
mds.2018$location <- location
mds.2019$location <- location
rownames(mds.2015) <- NULL
rownames(mds.2016) <- NULL
rownames(mds.2017) <- NULL
rownames(mds.2018) <- NULL
rownames(mds.2019) <- NULL
mds.all <- rbind(mds.2015, mds.2016, mds.2017, mds.2018, mds.2019)
colnames(mds.all) <- c("x", "y", "year", "location")


# 전체 시각화.
library(ggplot2)
p <- ggplot(data=mds.all, mapping = aes(x=x, y=y)) + 
  theme_void() +
  ggtitle("2015~2019년 지역별 유사도 5대 범죄") +
  theme(plot.title = element_text(family = "NanumGothic", face = "bold", hjust = 0.5, size = 15, color = "black")) +
  scale_color_discrete(name="연도") +
  geom_text(aes(label=location, color=as.factor(year)))
p

# 그래프 애니메이션.
# install.packages('gganimate')
library(gganimate)
p <- p + transition_time(year) + 
  labs(title = "Year : {round(frame_time, 0)}") +
  enter_fade()
  
p
anim_save('population.gif')
animate(p, start_pause = 25, end_pause = 25, height = 800, width = 800)

