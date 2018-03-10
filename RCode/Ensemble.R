#####################################################################################
### Project : Ensemble modeling
### Script : Ensemble.R
### Description : Applying Ensemble modeling to the engine data
#####################################################################################

#####################################################################################
### Setting up Environment
#####################################################################################

# Load library  
pkgs <- c("DJL", "randomForest", "adabag", "caret", "caretEnsemble", "MASS", "kernlab")
sapply(pkgs, require, character.only = T)

# Data
df <- dataset.engine.2015[, -c(1, 2)]

# Parameter
id.train <- sample(1:nrow(df), 1000)                                     # training set index
depth    <- autoprune(Type ~ ., data = df[id.train,])$control$maxdepth   # calc optimal depth


#####################################################################################
### Bagging
#####################################################################################

# Single train-test
train.bag    <- bagging(Type ~ ., data = df[id.train,], mfinal = 10, 
                        control = rpart.control(maxdepth = depth))            # train w optimal depth
cfusion.bag  <- table(as.factor(train.bag$class), df$Type[id.train])          # confusion matrix on training set
error.bag    <- sum(train.bag$class != df$Type[id.train]) / length(id.train)  # error rate on training set
plot.errorevol(errorevol(train.bag, newdata = df[-id.train,]),                # error plot over the number of trees (iteration)
               errorevol(train.bag, newdata = df[id.train,]))                
par(mfrow = c(1, 1), mar=c(1, 0, 1, 0))                                       # plot individual tree
plot(train.bag$trees[[1]], uniform = T)
text(train.bag$trees[[1]], use.n = T, all = T, cex = .8)

test.bag     <- predict.bagging(train.bag, newdata = df[-id.train,])          # test
error.e.bag  <- sum(test.bag$class != df$Type[-id.train]) / length(-id.train) # error rate on test set

# Cross-validation
cv.bag       <- bagging.cv(Type ~ ., v = 5, data = df, mfinal = 10,
                           control = rpart.control(maxdepth = depth))

# Random Forests
train.rf <- randomForest(Type ~ ., data = df[id.train,], ntree = 10)
importance(train.rf)
cv.rf    <- rfcv(df[,1:5], df$Type, cv.fold = 5)


#####################################################################################
### Boosting
#####################################################################################

# Single train-test
train.boo    <- boosting(Type ~ ., data = df[id.train,], mfinal = 10, 
                         control = rpart.control(maxdepth = depth))          # train w optimal depth
cfusion.boo  <- table(as.factor(train.boo$class), df$Type[id.train])         # confusion matrix on training set
error.boo    <- sum(train.boo$class != df$Type[id.train]) / length(id.train) # error rate on training set

plot.errorevol(errorevol(train.boo, newdata = df[-id.train,]),               # error plot over the number of trees (iteration)
               errorevol(train.boo, newdata = df[id.train,]))
margin.r.boo <- margins(train.boo, df[id.train,])$margins                    # margins: -(mis-classified) ~ 0(uncertainly) ~ 1(certainly)

par(mfrow = c(1, 1), mar=c(1, 0, 1, 0))                                      # plot individual tree
plot(train.boo$trees[[1]], uniform = T)
text(train.boo$trees[[1]], use.n = T, all = T, cex = .8)

test.boo     <- predict.boosting(train.boo, newdata = df[-id.train,])        # test
margin.e.boo <- margins(test.boo, df[-id.train,])$margins                    # margins: -(mis-classified) ~ 0(uncertainly) ~ 1(certainly)

# Cross-validation (v-fold)
cv.boo       <- boosting.cv(Type ~ ., v = 5, data = df, mfinal = 10,
                            control = rpart.control(maxdepth = depth))

# Benchmark
data.frame(SVM           = sprintf("%1.2f%%", cv.svm$tot.accuracy), 
           Bagging       = sprintf("%1.2f%%", (1 - cv.bag$error)*100), 
           RandomForest  = sprintf("%1.2f%%", (1 - cv.rf$error[1])*100), 
           Boosting      = sprintf("%1.2f%%", (1 - cv.boo$error)*100))


#####################################################################################
### Stacking
#####################################################################################

# Binning
df.bin      <- df[df$Type == "NA-P" | df$Type == "TC-P",]                  # caretEnsemble only takes 2 classes
df.bin$Type <- ifelse(df.bin$Type == "NA-P", "na.p", "tc.p")               # so let's play with NA-P and TC-P case only

# Individual runs of various methods
st.methods <- c("lda", "rpart", "glm", "knn", "svmRadial")                 # base learners
st.control <- trainControl(method = "cv", number = 5,                      # validation setups
                           savePredictions = T, classProbs = T)
st.models  <- caretList(Type ~., data = df.bin,                            # Base learn
                        trControl = st.control, methodList = st.methods)

# Meta learn using Ramdom Forest
stack.rf <- caretStack(st.models, method = "rf",                         # combine base learners
                       metric = "Accuracy", trControl = st.control)
print(stack.rf)

# Benchmark
data.frame(LDA      = sprintf("%1.2f%%", st.models$lda$results$Accuracy*100), 
           RPart    = sprintf("%1.2f%%", st.models$rpart$results$Accuracy[1]*100),
           GLM      = sprintf("%1.2f%%", st.models$glm$results$Accuracy[1]*100),
           KNN      = sprintf("%1.2f%%", st.models$knn$results$Accuracy[3]*100),
           SVM.RBF  = sprintf("%1.2f%%", st.models$svmRadial$results$Accuracy[3]*100),
           Stacking = sprintf("%1.2f%%", stack.rf$error$Accuracy[3]*100))

