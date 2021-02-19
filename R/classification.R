#decision tree

library(party)
library(lattice)
library(caret)

a<-read.csv("S:/Users/Dain/R/energy/data/data.1.csv",header=T)
a
#가중치 곱한것
a$average.temp1 <- a$average.temp*a$weight
a$lowtemp1 <- a$lowtemp*a$weight 	
a$high.temp1<-a$high.temp*a$weight
a$dayrain1<-a$dayrain*a$weight
a$average.wind1<-a$average.wind*a$weight
a$low.humidity1<-a$low.humidity*a$weight
a$average.humidity1<-a$average.humidity*a$weight
a$sumdaylighthours1<-a$sumdaylighthours*a$weight
a$holiday1<- 1*a$holiday
#같은날짜별로 평균낸것/ 하나의도시처럼 만든것
b<-aggregate(a[,14:22], list(day=a$day),mean)

#temp.data
a<-read.csv("S:/Users/Dain/R/energy/data/trade_raw1.csv",header=T)
a
# a데이터 갯수 세기
x<-seq(1:dim(a)[1])

fit.lm<-lm(a[,3]~x)

temp.data<-a

#데이터합치기
x<-cbind(temp.data,b[2:10])
x[is.na(x)] <- 0
sum(is.na(x))

# iris 데이터를 7:3의 비율로 trainData와 testData로 샘플링한다.
set.seed(1234)
ind <- sample(2, nrow(x), replace = TRUE, prob = c(0.3, 0.7))
trainData <- x[ind == 1, ]
testData <- x[ind == 2, ]

##naive bayes
library(e1071)

nBayes <- naiveBayes(trainData[,2:35], trainData[,1])
pred_bayes<-predict(nBayes, testData[,2:24])
testData[,24]
table(pred_bayes, testData[,25])

## rf
library(randomForest)
rf <- randomForest(temp.data[,1], data=trainData, ntree=100, proximity=TRUE)
table(predict(rf), trainData$Species)
print(rf)
