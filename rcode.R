setwd("C:/Users/sebas/Desktop/analytics_hire")
train_data = read.csv("train.csv")

plot(train_data$windspeed,train_data$electricity_consumption)


lm_1 = lm(electricity_consumption ~ temperature+
            var1+pressure+windspeed+var2+
            year+hour+month,data = train_data)
summary(lm_1)

test_data = read.csv("test.csv")
test_data$p = predict(lm_1,test_data)

write.csv(test_data,file="test_predict.csv")

##################################################################
lm_2 = lm(log(electricity_consumption) ~ temperature+
            var1+log(pressure)+log(windspeed)+var2+
            year+hour+month,data = train_data)
summary(lm_1)
str(train_data)
test_data = read.csv("test.csv")
test_data$p = predict(lm_2,test_data)

write.csv(test_data,file="test_predict_2.csv")
####################################################################

library(randomForest)

p = randomForest(electricity_consumption ~ temperature+
                   var1+pressure+windspeed+var2+
                   year+hour+month,data = train_data,
                 ntree=501, mtry =3, nodesize = 250,importance=TRUE
                )
test_data = read.csv("test.csv")
test_data$p_n = predict(p,test_data)

write.csv(test_data,file="test_predict_3.csv")

str(train_data)
tRF <- tuneRF(x = train_data[,c(-11,-1,-2)], 
              y=train_data$electricity_consumption,
              mtryStart = 3, 
              ntreeTry=501, 
              stepFactor = 2, 
              improve = 0.001, 
              trace=FALSE, 
              plot = FALSE,
              doBest = TRUE,
              nodesize = 270, 
              importance=FALSE
)

test_data$p_n = predict(lm_1,test_data)

write.csv(test_data,file="test_predict_6.csv")

###############################################################

library(nnet)
library(neuralnet)
train_n = read.csv("train_n.csv")
nn1 = nnet(formula = electricity_consumption ~ year+
             month,data = train_n,size = 5,linear.output = TRUE)

test_new = read.csv("test_n.csv")

test_data$p_n = predict(nn1$residuals,test_new)

write.csv(test_data,file="test_predict_8.csv")
nn1$residuals
