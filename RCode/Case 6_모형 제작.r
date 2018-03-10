#####################################################################################
### Stacking
#####################################################################################

## �ӻ�� ���� �� ���� ������ factor���� ����� �ν� ���Ͽ� �ٽ� ���������� ĳ���� 
bank_impute$job <- as.character(bank_impute$job)
bank_impute$marital <- as.character(bank_impute$marital)
bank_impute$education <- as.character(bank_impute$education)
bank_impute$loan <- as.character(bank_impute$loan)
bank_impute$default <- as.character(bank_impute$default)
bank_impute$contact <- as.character(bank_impute$contact)
bank_impute$poutcome <- as.character(bank_impute$poutcome)
bank_impute$y <- as.character(bank_impute$y)
bank_impute$pdays_cat <- as.character(bank_impute$pdays_cat)


#### ������ �ڵ� �̿�
df.bin <- bank_impute
str(df.bin)
df.bin$y <- ifelse(df.bin$y=="1", "Yes", "No") ## y���� �ʼ��� �ٲ�� �� (caretList �Լ� ��� �Ұ� ����)
df.bin$y <- as.factor(df.bin$y) ## y ���� ����ȯ 

## �����ͼ� �и� 
set.seed(1234)
id.train <- sample(1:nrow(df.bin), nrow(df.bin)*0.8) 
df.test <- df.bin[-id.train,]
df.bin <- df.bin[id.train,]

## Class Unbalance ���� - Upsampling ��� 
table(df.train$y)
df.train <- upSample(subset(df.bin,select=-y), df.bin$y)
nrow(df.train))
nrow(unique(df.train))
table(df.train$Class)
names(df.train)[length(names(df.train))] <- "y"

# Individual runs of various methods
st.methods <- c("rpart", "nnet", "glm", "lda", "knn")                 # base learners 
st.control <- trainControl(method = "cv", number = 5,                      # validation setups
                           savePredictions = T, classProbs = T)
st.models  <- caretList(as.factor(y) ~., data = df.bin,                            # Base learn
                        trControl = st.control, methodList = st.methods)
#st.models$rpart


# Meta learn using Ramdom Forest
stack.nnet <- caretStack(st.models, method = "rf",                         # combine base learners.. Random Forest�� ��Ȯ�� �߽��� stack ������ ����� 
                        metric = "Accuracy", trControl = st.control)
print(stack.rf)

# �ٸ� ����� Ȱ���� �ӻ�� ���� ���� 
stack.nnet <- caretStack(st.models, method = "nnet",                         # combine base learners.. Neural Network�� ��Ȯ�� �߽��� stack ������ ����� 
                       metric = "Accuracy", trControl = st.control)
stack.rpart <- caretStack(st.models, method = "rpart",                         # combine base learners.. Decision Tree(CART)�� ��Ȯ�� �߽��� stack ������ ����� 
                       metric = "Accuracy", trControl = st.control)
stack.glm <- caretStack(st.models, method = "glm",                         # combine base learners.. GLM(������ƽ ȸ�ͺм�)�� ��Ȯ�� �߽��� stack ������ ����� 
                       metric = "Accuracy", trControl = st.control)
stack.nb <- caretStack(st.models, method = "nb",                         # combine base learners.. Naive Bayes�� ��Ȯ�� �߽��� stack ������ ����� 
                       metric = "Accuracy", trControl = st.control)
stack.qda <- caretStack(st.models, method = "qda",                         # combine base learners.. QDA�� ��Ȯ�� �߽��� stack ������ ����� 
                       metric = "Accuracy", trControl = st.control)
stack.lda <- caretStack(st.models, method = "lda",                         # combine base learners.. LDA�� ��Ȯ�� �߽��� stack ������ ����� 
                       metric = "Accuracy", trControl = st.control)
stack.knn <- caretStack(st.models, method = "knn",                         # combine base learners.. KNN�� ��Ȯ�� �߽��� stack ������ ����� 
                        metric = "Accuracy", trControl = st.control)

## 1�� ������ Ȱ���� ���� 
pred.rpart <- predict(st.models$rpart, df.test)
pred.nnet <- predict(st.models$nnet, df.test)
pred.glm <- predict(st.models$glm, df.test)
pred.lda <- predict(st.models$lda, df.test)
pred.knn <- predict(st.models$knn, df.test)

## �ӻ�� ������ Ȱ���� ���� 
pred.stack <- predict(stack.rf, df.test)
pred.stack.nnet <- predict(stack.nnet, df.test)
pred.stack.rpart <- predict(stack.rpart, df.test)
pred.stack.glm <- predict(stack.glm, df.test)
pred.stack.nb <- predict(stack.nb, df.test)
pred.stack.qda <- predict(stack.qda, df.test)
pred.stack.lda <- predict(stack.lda, df.test)
pred.stack.knn <- predict(stack.knn, df.test)


## ���� ��� 
confusionMatrix(pred.nnet, df.test$y, positive="Yes")
confusionMatrix(pred.rpart, df.test$y, positive="Yes")
confusionMatrix(pred.glm, df.test$y, positive="Yes")
confusionMatrix(pred.lda, df.test$y, positive="Yes")
confusionMatrix(pred.knn, df.test$y, positive="Yes")
confusionMatrix(pred.stack, df.test$y, positive="Yes")

## �ӻ�� �𵨺� 
confusionMatrix(pred.stack, df.test$y, positive="Yes")
confusionMatrix(pred.stack.nnet, df.test$y, positive="Yes")
confusionMatrix(pred.stack.rpart, df.test$y, positive="Yes")
confusionMatrix(pred.stack.glm, df.test$y, positive="Yes")
confusionMatrix(pred.stack.nb, df.test$y, positive="Yes")
confusionMatrix(pred.stack.qda, df.test$y, positive="Yes")
confusionMatrix(pred.stack.lda, df.test$y, positive="Yes")
confusionMatrix(pred.stack.knn, df.test$y, positive="Yes")

# Train set ��Ȯ�� 
data.frame(NNET     = sprintf("%1.2f%%", st.models$nnet$results$Accuracy[length(st.models$nnet$results$Accuracy)]*100), 
           RPart    = sprintf("%1.2f%%", st.models$rpart$results$Accuracy[1]*100),
           GLM      = sprintf("%1.2f%%", st.models$glm$results$Accuracy[1]*100),
           LDA      = sprintf("%1.2f%%", st.models$lda$results$Accuracy[1]*100),
           KNN      = sprintf("%1.2f%%", st.models$knn$results$Accuracy[3]*100),
           Stacking = sprintf("%1.2f%%", stack.rf$error$Accuracy[3]*100))

# Test set ��Ȯ�� 
data.frame(NNET     = sprintf("%1.2f%%", confusionMatrix(pred.nnet, df.test$y)$overall[1]*100), 
           RPart    = sprintf("%1.2f%%", confusionMatrix(pred.rpart, df.test$y)$overall[1]*100),
           GLM      = sprintf("%1.2f%%", confusionMatrix(pred.glm, df.test$y)$overall[1]*100),
           LDA      = sprintf("%1.2f%%", confusionMatrix(pred.lda, df.test$y)$overall[1]*100),
           KNN      = sprintf("%1.2f%%", confusionMatrix(pred.knn, df.test$y)$overall[1]*100),
           Stacking = sprintf("%1.2f%%", confusionMatrix(pred.stack, df.test$y)$overall[1]*100))


###### Upsampling�� ���� ��� �� 
## Upsampling ���� ���� ����
st.models2  <- caretList(as.factor(y) ~., data = df.bin,                            # Base learn 
                        trControl = st.control, methodList = st.methods)
# model 1,2�� ���̴� Ȯ������ or not 

## Meta learn using Ramdom Forest
stack.rf2 <- caretStack(st.models2, method = "rf",                         # combine base learners.. Random Forest�� ��Ȯ�� �߽��� stack ������ ����� 
                       metric = "Accuracy", trControl = st.control)
print(stack.rf)

## 1�� ���� �� �ӻ�� ������ Ȱ���� ���� 
pred.rpart2 <- predict(st.models2$rpart, df.test)
pred.nnet2 <- predict(st.models2$nnet, df.test)
pred.glm2 <- predict(st.models2$glm, df.test)
pred.lda2 <- predict(st.models2$lda, df.test)
pred.knn2 <- predict(st.models2$knn, df.test)
pred.stack2 <- predict(stack.rf2, df.test)


## ���� ��Ȯ�� �� 
confusionMatrix(pred.nnet2, df.test$y, positive="Yes")
confusionMatrix(pred.rpart2, df.test$y, positive="Yes")
confusionMatrix(pred.glm2, df.test$y, positive="Yes")
confusionMatrix(pred.lda2, df.test$y, positive="Yes")
confusionMatrix(pred.knn2, df.test$y, positive="Yes")
confusionMatrix(pred.stack2, df.test$y, positive="Yes")

# Train Set
data.frame(NNET     = sprintf("%1.2f%%", st.models2$nnet$results$Accuracy[length(st.models$nnet$results$Accuracy)]*100), 
           RPart    = sprintf("%1.2f%%", st.models2$rpart$results$Accuracy[1]*100),
           GLM      = sprintf("%1.2f%%", st.models2$glm$results$Accuracy[1]*100),
           LDA      = sprintf("%1.2f%%", st.models2$lda$results$Accuracy[1]*100),
           KNN      = sprintf("%1.2f%%", st.models2$knn$results$Accuracy[3]*100),
           Stacking = sprintf("%1.2f%%", stack.rf2$error$Accuracy[3]*100))

# Test Set
data.frame(NNET     = sprintf("%1.2f%%", confusionMatrix(pred.nnet2, df.test$y)$overall[1]*100), 
           RPart    = sprintf("%1.2f%%", confusionMatrix(pred.rpart2, df.test$y)$overall[1]*100),
           GLM      = sprintf("%1.2f%%", confusionMatrix(pred.glm2, df.test$y)$overall[1]*100),
           LDA      = sprintf("%1.2f%%", confusionMatrix(pred.lda2, df.test$y)$overall[1]*100),
           KNN      = sprintf("%1.2f%%", confusionMatrix(pred.knn2, df.test$y)$overall[1]*100),
           Stacking = sprintf("%1.2f%%", confusionMatrix(pred.stack2, df.test$y)$overall[1]*100))

