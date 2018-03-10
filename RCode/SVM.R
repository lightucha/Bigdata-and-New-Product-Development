#####################################################################################
### Project : Support Vector Machines
### Script : SVM.R
### Description : Applying SVM to the engine data
#####################################################################################

#####################################################################################
### Setting up Environment
#####################################################################################

# Load library  
pkgs <- c("DJL", "e1071")
sapply(pkgs, require, character.only = T)

# Data
df <- dataset.engine.2015[, -c(1, 2)]


#####################################################################################
### Holdout test (single run)
#####################################################################################

# Single train-test / predetermined parameters: cost & gamma
id.train        <- sample(1:nrow(df), 1000)                                      # training set index
train.svm       <- svm(Type ~ ., data = df[id.train,], gamma = 0.1, cost = 500)  # train w a predefined C & gamma
cfusion.r.svm   <- table(train.svm$fitted, df$Type[id.train])                    # confusion matrix on training set
error.r.svm     <- sum(train.svm$fitted != df$Type[id.train]) / length(id.train) # error rate on training set
test.svm        <- predict(train.svm, newdata = df[-id.train,])                  # test
cfusion.e.svm   <- table(test.svm, df$Type[-id.train])                           # confusion matrix on test set
error.e.svm     <- sum(test.svm != df$Type[-id.train]) / length(-id.train)       # error rate on test set


# Single train-test / grid search to determin the optimal parameters: cost & gamma
tune.svm        <- tune.svm(Type ~ ., data = df[id.train,], 
                            gamma = seq(0.04, 0.16, 0.02),
                            cost = seq(300, 600, 50))                            # train w a range of C & gamma
plot(tune.svm)                                                                   # Error landscape; update cost/gamma if necessary
tuned.svm       <- svm(Type ~ ., data = df[id.train,], 
                       gamma = tune.svm$best.parameters[[1]],
                       cost = tune.svm$best.parameters[[2]])                     # tuned svm model on training set
cfusion.r.svm.u <- table(tuned.svm$fitted, df$Type[id.train])                    # confusion matrix on training set
error.r.svm.u   <- sum(tuned.svm$fitted != df$Type[id.train]) / length(id.train) # error rate on training set
test.svm.u      <- predict(tuned.svm, newdata = df[-id.train,])                  # test
cfusion.e.svm.u <- table(test.svm.u, df$Type[-id.train])                         # confusion matrix on test set
error.e.svm.u   <- sum(test.svm.u != df$Type[-id.train]) / length(-id.train)     # error rate on test set


#####################################################################################
### Cross-validation
#####################################################################################

# Cross-validation using the optimal parameters
cv.svm          <- svm(Type ~ ., data = df, cross = 5,
                       gamma = tune.svm$best.parameters[[1]],
                       cost = tune.svm$best.parameters[[2]])                     # 5-fold cross-validation on the whole df
summary(cv.svm)
