#install.packages("tidyverse")
library(tidyverse)
library(tidyr)
#install.packages("dplyr")
library(dplyr)
#install.packages("DMwR")
library(DMwR)
library(corrplot)
#install.packages("caret")
library(caret)

data <- read.csv("peaches_100_ALL_2.csv", sep=";",dec=",",header=TRUE)%>%
  mutate(Zrelost = as.factor(ifelse(firm < 3, "Zrelo", "Nezrelo")))

data<-data %>% filter(row_number() <= n()-5)

korelacija <- data[, c(2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]
corrplot(cor(korelacija), method="number")


data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)



set.seed(200)

library("ROSE")

validation_index<-caret::createDataPartition(data$Zrelost, p =0.80, list = FALSE)
test<-data[-validation_index, ]


test$firm<-NULL
train<-data[validation_index, ]
train$firm<-NULL
train$WI_CIE2<-NULL
train$theta<-NULL
train$zs<-NULL


table(data$Zrelost)

data_rose<-ROSE(Zrelost ~ .,data=train,seed=123)$data

table(data_rose$Zrelost)


#Logistička regresija na nebalansiranim podacima

log_reg<-glm(Zrelost ~ .,data=train, family=binomial)
predict_data<-predict(log_reg, newdata=test, type="response")
roc.curve(test$Zrelost, predict_data,plotit=FALSE)


#treniranje na balansiranim podacima

log_reg_bal<-glm(Zrelost ~ ., data=data_rose, family=binomial) 
predict_data_bal<-predict(log_reg_bal, newdata=test, type="response")
roc.curve(test$Zrelost, predict_data_bal,plotit=FALSE)

#provjera točnosti



#slučajne šume

library(randomForest)


library(caret)

rf <- randomForest(Zrelost~., data=train, proximity=TRUE) 
print(rf)

randomForest(formula = Zrelost ~ ., data = train)

p1 <- predict(rf, train)
confusionMatrix(p1, train$ Zrelost)

p2 <- predict(rf, test)
confusionMatrix(p2, test$ Zrelost)

plot(rf)

# na balansiranim

rf <- randomForest(Zrelost~., data=data_rose, proximity=TRUE) 
print(rf)

randomForest(formula = Zrelost ~ ., data = data_rose)

p1 <- predict(rf, data_rose)
confusionMatrix(p1, data_rose$ Zrelost)


# Modeli objašnjenja
 
library("randomForest")
library("DALEX")

num_1<- data[1,]

explain_rf <- DALEX::explain(model = rf,  
                             data = data,
                             y = data$Zrelost == "Zrelo", 
                             label = "Random Forest")



bd_rf <- predict_parts(explainer = explain_rf,
                       new_observation = num_1,
                      type = "break_down")

bd_rf

plot(bd_rf)

num_54<- data[54,]


bd_rf2 <- predict_parts(explainer = explain_rf,
                        new_observation = num_54,
                        type = "break_down")

bd_rf2
plot(bd_rf2)


#Local-diagnostics Plots
explain_rf <- DALEX::explain(model = rf,  
                             data = data_rose,
                             y = data_rose$Zrelost == "Zrelo", 
                             label = "Random Forest")

install.packages("DALEXtra")  
install.packages("lime")
library("DALEXtra")
library("lime")
model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer
num_1<- data[1,]
lime_num_1 <- predict_surrogate(explainer = explain_rf, 
                                new_observation = num_1, 
                                n_features = 3, 
                                n_permutations = 1000,
                                type = "lime")
(as.data.frame(lime_num_1))

plot(lime_num_1)

