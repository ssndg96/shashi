setwd("D:/base-analytics-master/base analytics 2nd time")
View(house_prices)
df<-read.csv("house_prices.csv")

library(ggplot2)
ggplot(df)
geom_point(aes(X~SqFt,Y~Price,color~Brick),size=4)
View(df)

fit<-lm(Price ~ SqFt +  Neighborhood + Brick +Bedrooms+Offers,data =df)
summary(fit)

df$pred <- predict(fit)

ggplot(df)
geom_point(aes(x=pred,y=Price),size=4)

t.test(df$Price ~ df$Brick)
boxplot(df$Price ~ df$Brick)

summary(aov(df$Price ~ df$Neighborhood))
boxplot(df$Price ~ df$Neighborhood)

cor(df$Price , df$Offers)
cor(df$Price , df$Bedrooms)
cor(df$Price , df$SqFt)
cor(df$Price , df$Bathrooms)
cor(df$SqFt , df$Price)
cor(df$SqFt , df$Bathrooms)
cor(df$SqFt , df$Bedrooms)
cor(df$SqFt , df$Offers)


x<- rnorm(1000)
y<- rnorm(1000)

cor(x,y)
plot(x,y)


x<- rnorm(1000)
y<- rnorm(1000)
x[200] <-1000
y[200] <-1000
cor(x,y,method = "spearman")
plot(x,y)


#split data into train and test
set.seed(100)
index <- sample(1:nrow(df),round(0.8*nrow(df)))
train<-df[index, ]
test<-df[-index, ]


#model selection

fit1<- lm(Price ~ SqFt+Neighborhood+Bedrooms,data=train)

train$pred<-predict(fit1)
test$pred <- predict(fit1,newdata=test)

train$res<-train$pred - train$Price
test$res <-test$pred - test$Price

hist(train$res)
hist(test$res)

plot(train$pred , train$res)


summary(fit1)
cor(test$pred,test$Price)^2




#pred<-predict(fit1,test)
#head(pred)
#head(test)



#muticolliearity
install.packages("car")
library(car)

vif(fit1)

#cooks distance
train$cookd <- cooks.distance(fit1)
hist(train$cookd,breaks = 30)

#4/N-K-1
train_new <-train[train$cookd < (4/(102-3-1)),]
View(train)

#step wise

library(MASS)
fit_null <- lm(Price ~ 1,data = df)
fit_all <- lm(Price ~ . ,data = df)
summary(fit_null)
summary(fit_all)
#forward
final_fit <- stepAIC(fit_null,
                     scope = list(lower=fit_null,upper=fit_all),
                     direction = "forward")
#backward
final_fit <- stepAIC(fit_all,
                     scope = list(lower=fit_null,upper=fit_all),
                     direction = "backward")
#combined
final_fit <- stepAIC(fit_null,
                     scope = list(lower=fit_null,upper=fit_all),
                     direction = "both")

#fit1<- lm(Price ~ SqFt+Bathrooms,data=train)
#summary(fit1)
#pred<-predict(fit1,test)
#head(pred)
#head(test)


#fit1<- lm(Price ~ SqFt+Bathrooms+Bedrooms,data=train)
#summary(fit1)
#pred<-predict(fit1,test)
#head(pred)
#head(test)

#fit1<- lm(Price ~ SqFt+Bathrooms+Bedrooms+Offers,data=train)
#summary(fit1)
#pred<-predict(fit1,test)
#head(pred)
#head(test)


#fit1<- lm(Price ~Bathrooms+Bedrooms+Offers+Neighborhood,data=train)
#summary(fit1)
#pred<-predict(fit1,test)
#head(pred)
#head(test)

