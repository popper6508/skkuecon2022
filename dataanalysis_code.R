install.packages("ggplot2")
install.packages('stargazer')
install.packages("ineq")
install.packages("psych")
install.packages("vars")
install.packages("forecast")
install.packages("car")
install.packages("moments")
install.packages("corrplot")

library(stargazer)
library(ggplot2)
library(dplyr)
library(rvest)
library(httr)
library(data.table)
library(stringr)
library(readr)
library(xts)
library(lubridate)
library(timetk)
library(gridExtra)
library(jsonlite)
library(xml2)
library(ineq)
library(psych)
library(vars)
library(forecast)
library(car)
library(moments)
library(corrplot)


setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#__# 타당성 재조사
data_re <- read.csv("./re_data.csv")

c_n <- c("Name","b_eco", "b_pol", "b_re", "b_rec", "a_eco","a_pol","a_re","a_rec")
colnames(data_re) <- c_n
data_re

before <- data_re[,c(1,2,3,4,5)]
after <- data_re[,c(1,6,7,8,9)]

time1 <- rep("개정 전", 7)
time2 <- rep("개정 후", 7)

before <- cbind(before, time1)
after <- cbind(after, time2)

c <- c("Name","경제성 가중치", "정책성 가중치", "지역균형발전 가중치", "지역낙후도 가중치", "시기")
colnames(before) <- c
colnames(after) <- c


data_f <- rbind(before,after)
data_f

ggplot(data = data_f, mapping = aes(x=시기, y=`지역균형발전 가중치`, color=시기)) +
  geom_boxplot(outlier.colour = "black", outlier.size = 4) +
  stat_summary(fun="mean", geom="point", shape=22, size=2, fill="blue")  +
  geom_jitter(shape=16, position=position_jitter(0.2))

ggplot(data = data_f, mapping = aes(x=시기, y=`지역낙후도 가중치`, color=시기)) +
  geom_boxplot(outlier.colour = "black", outlier.size = 4) +
  stat_summary(fun="mean", geom="point", shape=22, size=2, fill="blue")  +
  geom_jitter(shape=16, position=position_jitter(0.2))

#__# 전체 통계분석

raw_data <- read.csv("./raw_data.csv")
colnames(raw_data) <- c("연도", "사업", "경제성 평가 가중치", "정책성 평가 가중치", "지역균형발전 가중치", "지역낙후도 가중치","B/C", "AHP","시기")

ggplot(data = raw_data, mapping = aes(x=시기, y=`지역균형발전 가중치`, color=시기)) +
  geom_boxplot(outlier.colour = "black", outlier.size = 4) +
  stat_summary(fun="mean", geom="point", shape=22, size=2, fill="blue")  +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_x_discrete(limit = c("개정1 이전", "개정1", "개정2"))

ggplot(data = raw_data, mapping = aes(x=시기, y=`지역낙후도 가중치`, color=시기)) +
  geom_boxplot(outlier.colour = "black", outlier.size = 4) +
  stat_summary(fun="mean", geom="point", shape=22, size=2, fill="blue")  +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_x_discrete(limit = c("개정1 이전", "개정1", "개정2"))


#__# 시기별 AHP와 B/C의 관계
raw_data_1 <- raw_data[raw_data$시기=="개정1 이전" ,]
raw_data_2 <- raw_data[raw_data$시기=="개정1" ,]
raw_data_3 <- raw_data[raw_data$시기=="개정2" ,]

#1
ggplot(data = raw_data_1,mapping = aes(x=log(raw_data_1$`B/C`), y=AHP)) +
  geom_point(alpha =0.3, size = 2) +
  geom_smooth(method = "lm", colour = "steelblue")

lm.r1 <- lm(raw_data_1$AHP ~ log(raw_data_1$`B/C`), data = raw_data_1)
stargazer(lm.r1, type = 'text', keep.stat = c("n","rsq"))

#2
ggplot(data = raw_data_2,mapping = aes(x=log(raw_data_2$`B/C`), y=AHP)) +
  geom_point(alpha =0.3, size = 2) +
  geom_smooth(method = "lm", colour = "steelblue")

lm.r2 <- lm(raw_data_2$AHP ~ log(raw_data_2$`B/C`), data = raw_data_2)
stargazer(lm.r2, type = 'text', keep.stat = c("n","rsq"))


#3
ggplot(data = raw_data_3,mapping = aes(x=log(raw_data_3$`B/C`), y=AHP)) +
  geom_point(alpha =0.3, size = 2) +
  geom_smooth(method = "lm", colour = "steelblue")

lm.r3 <- lm(raw_data_3$AHP ~ log(raw_data_3$`B/C`), data = raw_data_3)
stargazer(lm.r3, type = 'text', keep.stat = c("n","rsq"))

#__# 연도별 각 가중치 비중 변화
raw_data <- raw_data %>% mutate(.,'지역낙후도/지역균형발전' = raw_data$`지역낙후도 가중치`/raw_data$`지역균형발전 가중치`)

yeas <- unique(raw_data$연도)
year_data <- data.frame()
cnt = 0

for (i in yeas) {
  cnt = cnt + 1
  x <- mean(raw_data[raw_data$연도==i,3])
  y <- mean(raw_data[raw_data$연도==i,4])
  z <- mean(raw_data[raw_data$연도==i,5])
  w <- mean(raw_data[raw_data$연도==i,6])
  w2 <- mean(raw_data[raw_data$연도==i,10])
  year_data[cnt,1:6] <- c(i,x,y,z,w,w2)
}
colnames(year_data) <- c("연도","경제성 평가 가중치", "정책성 평가 가중치", "지역균형발전 가중치", "지역낙후도 가중치","지역낙후도/지역균형발전")


year1 <- cbind(year_data$연도, year_data$`경제성 평가 가중치`, rep("경제성 평가 가중치", 18))
year2 <- cbind(year_data$연도, year_data$`정책성 평가 가중치`, rep("정책성 평가 가중치", 18))
year3 <- cbind(year_data$연도, year_data$`지역균형발전 가중치`, rep("지역균형발전 평가 가중치", 18))
year_data2 <- rbind(year1, year2, year3)
colnames(year_data2) <- c("연도", "가중치", "항목")

year_data2 <- as.data.frame(year_data2)
year_data2$가중치 <- as.numeric(year_data2$가중치)

ggplot(year_data2, mapping = aes(x=연도, y=가중치, group = 항목,color=항목)) +
  geom_line() +
  ylab("가중치") +
  geom_point(aes(x=연도, y=가중치, group = 항목,color=항목)) +
  scale_y_continuous(breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)) +
  theme(legend.position = c(0.85, 0.2))
  

ggplot(year_data, mapping = aes(x=연도)) +
  geom_line(aes(y=`지역낙후도/지역균형발전`))

#지역균형발전 비중과 AHP 관계
ggplot(data = raw_data_1,mapping = aes(x=raw_data_1$`지역균형발전 가중치`, y=AHP)) +
  geom_point(alpha =0.3, size = 2) +
  geom_smooth(method = "lm", colour = "steelblue") +
  xlab("지역균형발전 가중치")

lm.rr1 <- lm(raw_data_1$AHP ~ raw_data_1$`지역균형발전 가중치`, data = raw_data_1)
stargazer(lm.rr1, type = 'text', keep.stat = c("n","rsq"))

#2
ggplot(data = raw_data_2,mapping = aes(x=raw_data_2$`지역균형발전 가중치`, y=AHP)) +
  geom_point(alpha =0.3, size = 2) +
  geom_smooth(method = "lm", colour = "steelblue") +
  xlab("지역균형발전 가중치")

lm.rr2 <- lm(raw_data_2$AHP ~ raw_data_2$`지역균형발전 가중치`, data = raw_data_2)
stargazer(lm.rr2, type = 'text', keep.stat = c("n","rsq"))

#3
ggplot(data = raw_data_3,mapping = aes(x=raw_data_3$`지역균형발전 가중치`, y=AHP)) +
  geom_point(alpha =0.3, size = 2) +
  geom_smooth(method = "lm", colour = "steelblue") +
  xlab("지역균형발전 가중치")

lm.rr3 <- lm(raw_data_3$AHP ~ raw_data_3$`지역균형발전 가중치`, data = raw_data_3)
stargazer(lm.rr3, type = 'text', keep.stat = c("n","rsq"))

#4
ggplot(data = raw_data,mapping = aes(x=raw_data$`지역균형발전 가중치`, y=AHP)) +
  geom_point(alpha =0.3, size = 2) +
  geom_smooth(method = "lm", colour = "steelblue") +
  xlab("지역균형발전 가중치")

lm.r <- lm(raw_data$AHP ~ raw_data$`지역균형발전 가중치`, data = raw_data)
stargazer(lm.r, type = 'text', keep.stat = c("n","rsq"))







###################총괄 데이터
all_20 <- read.csv("데이터 모음.csv", fileEncoding="euc-kr")

###상관분석과 데이터 선별
cor.test(all_20$p2010.2021, all_20$gradu2010)
cor.test(all_20$p2010.2021, all_20$skilled2010)
cor.test(all_20$p2010.2021, all_20$manu2010)
cor.test(all_20$p2010.2021, all_20$service2010)
cor.test(all_20$p2010.2021, all_20$enov2010)
cor.test(all_20$p2010.2021, all_20$service2_2010)
cor.test(all_20$p2010.2021, all_20$manu2_2010)
cor.test(all_20$p2010.2021, all_20$enov2_2010)
cor.test(all_20$p2010.2021, all_20$young2010)
cor.test(all_20$p2010.2021, all_20$firm2010)

pointdata2010 <- all_20[,c(2:6, 10)]
pointdata2015 <- all_20[,c('gradu2015', 'skilled2015', 'service2015', 'manu2015', 'enov2015','young2015')]
pointdata2020 <- all_20[,c('gradu2020', 'skilled2020', 'service2020', 'manu2020', 'enov2020','young2020')]

###주성분분석 기반 경쟁력 지수
cor_111 <- cor(pointdata2010[1:6])
cor_111
cortest.bartlett(pointdata)
model1 <- lm(p2010.2015 ~ gradu2010 + skilled2010 + manu2010 + service2010 + enov2010 + young2010, data=all_20)
vif(model1) ##다중공선성이 너무 높아서 얘네들끼리 두고 다중회귀 분석하는 것은 적절하지 않다.

corrplot(cor_111, sig.level = 0.1, method = "square")

#주성분분석 기반 경쟁력 지수
prin_com <- princomp(pointdata2010[,1:6])
prin_com$loadings

pointdata2010[,7] <- (pointdata2010[,1]*0.234627 + pointdata2010[,2]*0.105539 +pointdata2010[,3]*0.200611 +
                        pointdata2010[,4]*0.180549 + pointdata2010[,5]*0.060619 + pointdata2010[,6]*0.218055)/0.35220971

prin_com2 <- princomp(pointdata2015[,1:6])
prin_com2$loadings

pointdata2015[,7] <- (pointdata2015[,1]*0.225515 + pointdata2015[,2]*0.094547 +pointdata2015[,3]*0.181178 +
                        pointdata2015[,4]*0.193492 + pointdata2015[,5]*0.061126 + pointdata2015[,6]*0.244503)/0.377272


prin_com3 <- princomp(pointdata2020[,1:6])
prin_com3$loadings

pointdata2020[,7] <- (pointdata2020[,1]*0.232054 + pointdata2020[,2]*0.100226 +pointdata2020[,3]*0.116930 +
                        pointdata2020[,4]*0.200451 + pointdata2020[,5]*0.0632054 + pointdata2020[,6]*0.287133)/0.415692


cor.test(pointdata2010$V7, all_20$p2010.2021)
cortest.bartlett(cbind(pointdata2010$V7, all_20$p2010.2021))
cor_point <- lm(all_20$p2010.2021~pointdata2010$V7)
stargazer(cor_point, type="text")
ggplot(mapping = aes(pointdata2010$V7, all_20$p2010.2021)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlab("지역 잠재력") +
  ylab("2010-2021 연평균인구증가율")


###불균등 분석
sd(pointdata2010$V7)

sd(pointdata2015$V7)

sd(pointdata2020$V7)

ineq(pointdata2010$V7)

ineq(pointdata2015$V7)

ineq(pointdata2020$V7)

quantile(pointdata2010$V7, 0.8)/quantile(pointdata2010$V7, 0.2)

quantile(pointdata2015$V7, 0.8)/quantile(pointdata2015$V7, 0.2)

quantile(pointdata2020$V7, 0.8)/quantile(pointdata2020$V7, 0.2)

write.csv(pointdata2010, file = "./2010.csv")
write.csv(pointdata2015, file = "./2015.csv")
write.csv(pointdata2020, file = "./2020.csv")

###경쟁력 지수 일관되게 적용 가능하도록 수정 (전국 평균으로 나눠버리기?)

###수도권 비수도권 격차
noncapital_2010 <- pointdata2010$V7[c(2,3,5,7,39:161)]
t.test(noncapital_2010, capital_2010)
t.test(noncapital_2010, m=0.977465)

noncapital_2015 <- pointdata2015$V7[c(2,3,5,7,39:161)]
t.test(noncapital_2015, capital_2015)
t.test(noncapital_2015, m=0.9953170)

noncapital_2020 <- pointdata2020$V7[c(2,3,5,7,39:161)]
t.test(noncapital_2020,capital_2020)
t.test(noncapital_2020, m=1.041992)


###개발촉진지구
x1111 <- read.csv("개발촉진.csv", fileEncoding="euc-kr")
x1111[,2:4] <- as.numeric(x1111[,2:4])

t.test(x1111$X2010[1:10], x1111$X2010[11:20])

t.test(x1111$X2020[1:10], x1111$X2020[11:20])

t.test(x1111$X2010, m=0.977465)

t.test(x1111$X2020, m=1.041992)
