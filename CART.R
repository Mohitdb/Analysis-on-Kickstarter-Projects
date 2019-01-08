rm(list=ls())
set.seed(123)
Startup_Raw<-read.csv("/Users/riaechhpal/Documents/CS 513/Startup_Raw.csv")


Startup_Raw1<-Startup_Raw[(!(Startup_Raw$state=="live") & !(Startup_Raw$state=="canceled") & !(Startup_Raw$state=="suspended") & (Startup_Raw$country=="US")),]
Startup_Raw1<-Startup_Raw1[,c(-1,-2,-3,-5,-7,-9,-12,-13)]

Startup_Raw1<-Startup_Raw1[(substring(Startup_Raw1$deadline,7,8)=="16"|substring(Startup_Raw1$deadline,7,8)=="17"|substring(Startup_Raw1$deadline,7,8)=="18"),]
Startup_Raw1<-Startup_Raw1[(substring(Startup_Raw1$launched,7,8)=="16"|substring(Startup_Raw1$launched,7,8)=="17"),]

Startup_Raw1$deadline <- strptime(as.character(Startup_Raw1$deadline), "%m/%d/%y")
Startup_Raw1$deadline <- as.Date(Startup_Raw1$deadline)

Startup_Raw1$launched <- strptime(as.character(Startup_Raw1$launched), "%m/%d/%y")
Startup_Raw1$launched <- as.Date(Startup_Raw1$launched)

library(zoo)

Startup_Raw1$launched <- as.yearqtr(Startup_Raw1$launched, format = "%Y-%m-%d")
Startup_Raw1$deadline <- as.yearqtr(Startup_Raw1$deadline, format = "%Y-%m-%d")

Startup_Raw1$deadline = as.character(Startup_Raw1$deadline)

Startup_Raw1 <- cbind(Startup_Raw1, do.call("rbind", strsplit(Startup_Raw1[, 2], " ")))

Startup_Raw1<- Startup_Raw1[, -2]

colnames(Startup_Raw1)[7] <- "d_year"
colnames(Startup_Raw1)[8] <- "d_quarter"

Startup_Raw1$launched = as.character(Startup_Raw1$launched)
Startup_Raw1 <- cbind(Startup_Raw1, do.call("rbind", strsplit(Startup_Raw1[,2], " ")))

Startup_Raw1<- Startup_Raw1[, -2]

colnames(Startup_Raw1)[8] <- "l_year"
colnames(Startup_Raw1)[9] <- "l_quarter"

Startup_Raw1$d_quarter<-as.numeric(factor(Startup_Raw1$d_quarter,levels=(c("Q1","Q2","Q3","Q4"))))
Startup_Raw1$l_quarter<-as.numeric(factor(Startup_Raw1$l_quarter,levels=(c("Q1","Q2","Q3","Q4"))))


Startup_Raw1$main_category<-as.numeric(factor(Startup_Raw1$main_category,levels=(c("Art","Comics","Crafts","Dance","Design","Fashion","Film & Video","Food","Games","Journalism","Music","Photography","Publishing","Technology","Theater"))))

Startup_Raw1$state<-as.numeric(factor(Startup_Raw1$state,levels=(c("failed","successful"))))
Startup_Raw1$state<-factor(Startup_Raw1$state,levels=(c("1","2")))


idx<-sort(sample(nrow(Startup_Raw1),as.integer(.75*nrow(Startup_Raw1))))
training<-Startup_Raw1[idx,]
test<-Startup_Raw1[-idx,]

library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

CART_class<-rpart( state~.,data=training,method="class")
prp(CART_class)
# summary(CART_class)
# text(CART_class)
# rpart.plot(CART_class)
# plotcp(CART_class)

CART_predict<-predict(CART_class,test, type='class')
table(Actual=test[,2],CART=CART_predict)
CART_wrong<-sum(test[,2]!=CART_predict)
CART_error_rate<-CART_wrong/length(test[,2])
CART_error_rate
#[1] 0.04897494