setwd("C:\\Users\\yoonjun\\Desktop\\jun\\4_1\\machine_learning\\논문\\논문 작성")
data <- read.csv('data.csv')

head(data)
str(data)

library(dplyr)
ts <- data %>%
  group_by(기간, 자치구) %>%
  summarize(합계 = sum(살인, 강도, 강간강제추행, 절도, 폭력)) 

library(ggplot2)
tsp <- ggplot(ts, aes(x=기간, y=합계, colour=자치구, group=자치구)) +
  geom_line(size=1) +
  geom_point(size=2) +
  ggtitle("Time series, 2014-2019 지역구별 범죄합") +
  theme(plot.title=element_text(size=20))

install.packages("plotly")
library(plotly)
ggplotly(tsp)

# 중구와 종로구가 구내 거주 인원 비례 범죄수가 다른 지역에 비해 높은 모습.
# 공통사항 : 추세가 비슷함.

    