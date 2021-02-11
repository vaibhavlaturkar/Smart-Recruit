


#########################
fintro_train <- fintro_replace[1:9527,]

fintro_train$Business_Sourced <- fintro$Business_Sourced

fintro_test <- fintro_replace[9528:14572,]


index1 <- sample(1:nrow(fintro_train),0.85*nrow(fintro_train))

training_set <- fintro_train[index1,]
testing_set <- fintro_train[-index1,]


############################################################################################################################### Logistic Model ########################################


colnames(training_set)

model_full <- glm(Business_Sourced~Manager_Grade+Manager_Status+Manager_Num_Application+Manager_Num_Coded+Age_Difference+Manager_Current_Designation+Manager_Business+Applicant_Occupation+Applicant_Gender+Applicant_Age,data = training_set,family = binomial(link = "cloglog"))


summary(model_full)
predict <- predict(model_full,testing_set,type="response")
range(predict)# 0.0000001478708 to 0.6158308

predict <- round(predict,digits = 4)
predict1 <- ifelse(predict>0.36,1,0)


auc(roc(testing_set$Business_Sourced,predict))





model_t <- glm(Business_Sourced~Manager_Grade+Manager_Status+Manager_Num_Application+Manager_Num_Coded+Age_Difference+Manager_Current_Designation+Manager_Business+Applicant_Occupation+Applicant_Gender+Applicant_Age,data = fintro_train)

fintro_test$Business_Sourced <- predict(model_t,fintro_test,type="response")

# Make a binary predictions-vector using a cut-off of 15%
fintro_test$Business_Sourced = ifelse(fintro_test$Business_Sourced > 0.15,1,0)

# Construct a confusion matrix
table_full <- table(test_set$loan_status,pred_cutoff_15)



write.csv(fintro_test,"logit.csv")


library(mboost)



##########################################################################################
###################################  Random Forest ######################################  
library(randomForest)




ranmodel <- randomForest(as.factor(Business_Sourced)~Manager_Grade+Manager_Status+Manager_Num_Application+Manager_Num_Coded+Age_Difference+Manager_Current_Designation+Manager_Business+Applicant_Occupation+Applicant_Gender+Applicant_Age,data = training_set,ntree=500,importance=TRUE,type="classification")

?randomForest

train <- training_set[,-c(10,16,3,6,1)]
colnames(training_set)

(imp <- importance(ranmodel)[,1])



ranpredict <- predict(ranmodel,testing_set)
auc(roc(testing_set$Business_Sourced,as.integer(ranpredict)))


head(fintro_test$Business_Sourced6 <- predict(ranmodel,fintro_test,type="prob"))



colnames(fintro_test)
write.csv(fintro_test[,c(1,35)],"rand_un.csv")




##########################################################################################
################################## Descision Tree #######################################

library(rpart)

modeltree <- rpart(Business_Sourced~Manager_Grade+Manager_Status+Manager_Num_Application+Manager_Num_Coded+Age_Difference+Manager_Current_Designation+Manager_Business+Applicant_Occupation+Applicant_Gender+Applicant_Age,data = training_set)

#  Prune the tree using tree_min
indextree <- which.min(modeltree$cptable[,"xerror"])
tree_min <- modeltree$cptable[indextree, "CP"]
ptree_prior <- prune(modeltree, cp = tree_min)

pred_prior <- predict(ptree_prior, newdata = testing_set)

auc(roc(testing_set$Business_Sourced,as.integer(pred_prior)))




write.csv(fintro_test[,c(1,34)],"tree1.csv")


#########################################################################################
##################################### Logit Boost ######################################

library(caTools)
colnames(training_set)
train <- training_set[,-30]
label <- training_set[,30]

train <- train[,c("Office_PIN","Applicant_Gender","Manager_Grade","Manager_Status","Manager_Num_Application","Manager_Num_Coded","Applicant_Occupation","Manager_Current_Designation","Manager_Business","Applicant_Age","Age_Difference")]



logitmodel <- LogitBoost(train,label,nIter=20)


##########################################################################################
####################################### SVM ############################################

library(e1071)

tuned <- tune.svm(Business_Sourced~Office_PIN+Applicant_Gender+Manager_Grade+Manager_Status+Manager_Num_Application+Manager_Num_Coded+Applicant_Occupation+Manager_Current_Designation+Manager_Business+Applicant_Age+Age_Difference, data = training_set, gamma = 10^(-6:-1), cost = 10^(1:2))

summary(tuned)


svmmodel <- svm(Business_Sourced~Office_PIN+Applicant_Gender+Manager_Grade+Manager_Status+Manager_Num_Application+Manager_Num_Coded+Applicant_Occupation+Manager_Current_Designation+Manager_Business+Applicant_Age+Age_Difference,data = fintro_train,kernel="radial",gamma=0.009)



svmpredict <- predict(svmmodel,testing_set)
range(svmpredict)

fintro_test$Business_Sourced8 <- predict(svmmodel,fintro_test)

auc(roc(testing_set$Business_Sourced,svmpredict))

write.csv(fintro_test[,c(1,37)],"svm2.csv")

colnames(fintro_test)


#################################### Xg Boost #########################################


