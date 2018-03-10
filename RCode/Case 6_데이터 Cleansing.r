pkgs <- c("plyr", "DJL", "e1071", "ROCR",
          "caret", "MASS", "rpart", "rattle", "adabag","randomForest", "caretEnsemble","kernlab", "mice", "DMwR")
tt <- sapply(pkgs, require, character.only=T)

## 패키지 설치 
for(i in 1:length(tt)) {
  if(tt[i]==FALSE) {
    install.packages(names(tt)[i])
  }
}

## 패키지 적용 
sapply(pkgs, require, character.only=T)

## 데이터 입력 
## 사전에 unknown은 미리 공백으로 조정 
bank <- read.csv("C:\\Users\\Hyunjin\\Dropbox\\2017년 1학기\\고급 데이터마이닝\\Data\\bank-additional\\bank-additional-full2.csv", header=T, stringsAsFactors=F)

## Yes, No 비율 
table(bank$y)

## 그래프 
boxplot(age ~ y, bank); summary(bank$age)
boxplot(bank$pdays[bank$pdays!=999] ~ bank$y[bank$pdays!=999]); summary(bank$pdays[bank$pdays!=999])
boxplot(previous ~ y, bank); summary(bank$previous)
boxplot(emp.var.rate ~ y, bank); summary(bank$emp.var.rate)
boxplot(cons.price.idx ~ y, bank); summary(bank$cons.price.idx)
boxplot(euribor3m ~ y, bank); summary(bank$euribor3m)
boxplot(nr.employed ~ y, bank); summary(bank$nr.employed)
jobtable <- as.matrix(table(bank$job, bank$y)); round(jobtable[,2]/(jobtable[,1]+jobtable[,2]),4)
edutable <- as.matrix(table(bank$education, bank$y)); round(edutable[,2]/(edutable[,1]+edutable[,2]),4)


str(bank)

## 쓸만한 변수를 찾아본다 
summary(glm(as.factor(loan)~as.factor(housing), data=bank, family="binomial")) ## 강한 상관성이 있음 // 차후 빚 관련 변수 하나로 축소 


## pass day 조정 
hist(subset(bank$pdays, bank$pdays!=999), main="pdays", xlab="경과일자", ylab="빈도")
summary(subset(bank$pdays, bank$pdays!=999))

bank$pdays_cat <- ifelse(bank$pdays==999, 0, 
                         ifelse(bank$pdays<=5, 1,
                                ifelse(bank$pdays<=10, 2,
                                       ifelse(bank$pdays<=15, 3, 4))))

## Job 범주 축소 by 순오누나 
bank$job <- ifelse(bank$job=="admin." | bank$job=="entrepreneur" | bank$job=="management" | bank$job=="services", "management", bank$job)

## Education 범주 축소 by 순오누나 
bank$education <- ifelse(bank$education=="basic.4y" | bank$education=="basic.6y" | bank$education=="basic.9y" | bank$education=="illiterate" | bank$education=="high.school", "le_hs", 
                         ifelse(bank$education=="professional.course" | bank$education=="university.degree", "ge_univ", bank$education))

str(bank)


bank_mod <- bank[,-which(names(bank) %in% c("month", "day_of_week", "pdays", "duration"))] ## month, day_of_week로 인사이트를 만들기 어려워 제외 
bank_mod$job <- ifelse(bank_mod$job=="", NA, bank_mod$job) ## 공백을 NA로 인식을 못 해서 수정
bank_mod$marital <- ifelse(bank_mod$marital=="", NA, bank_mod$marital)
bank_mod$education <- ifelse(bank_mod$education=="", NA, bank_mod$education)
head(bank_mod)

write.csv(bank_mod, "C:\\Users\\Hyunjin\\Dropbox\\2017년 1학기\\고급 데이터마이닝\\Data\\bank-additional\\bank-additional-full3.csv", row.names=F, na="")
## 제공용 

## 결측치 변수와 나머지 변수의 관계 
bank_mod2 <- bank_mod[!(rowSums( is.na( bank_mod[,which(names(bank_mod) %in% c("default", "housing", "loan"))] ) )==3),] ## default와 빚 변수간에는 밀접한 관계가 있을 것 같으니, 이거 3개가 전부 결측인 건 제외하자 
nrow(bank_mod2)
mean(!complete.cases(bank_mod2)) ## 결측치 다 지우면 25.57% 데이터 날라감 

miss1 <- as.data.frame(abs(is.na(bank_mod2)))
miss2 <- miss1[which(apply(miss1,2,sum)>0)]

cor(miss2) ## 결측값 있는 변수들의 결측여부 dummy 변수간 상관관계 

write.csv(cor(miss2), "C:\\Users\\Hyunjin\\Dropbox\\2017년 1학기\\고급 데이터마이닝\\Data\\bank-additional\\miss2_cor.csv", row.names=T, na="")
## 제공용 

miss3 <- cor(bank_mod2[,-c(2,3,4,5,6,7,8,11,17)], miss2, use="pairwise.complete.obs") ## 결측값 있는 변수들의 결측여부 dummy 변수와 나머지 변수 간의 상관관계 

write.csv(miss3, "C:\\Users\\Hyunjin\\Dropbox\\2017년 1학기\\고급 데이터마이닝\\Data\\bank-additional\\miss3_cor.csv", row.names=T, na="")
## 제공용 

## 빚 관련 변수 축소 
bank_mod3 <- bank_mod2[,-which(names(bank_mod2) %in% "housing")]
str(bank_mod3)

## 캐스팅 (안 하면 Imputation 제대로 안 됨)
bank_mod3$job <- as.factor(bank_mod3$job)
bank_mod3$marital <- as.factor(bank_mod3$marital)
bank_mod3$education <- as.factor(bank_mod3$education)
bank_mod3$loan <- as.factor(bank_mod3$loan)
bank_mod3$default <- as.factor(bank_mod3$default)
bank_mod3$contact <- as.factor(bank_mod3$contact)
bank_mod3$poutcome <- as.factor(bank_mod3$poutcome)
bank_mod3$pdays_cat <- as.factor(bank_mod3$pdays_cat)

## Multiple Imputation
imp.mice = mice(bank_mod3[,-which(names(bank_mod3) %in% "y")], m=5)
bank_impute <- data.frame(cbind(complete(imp.mice), data.frame(bank_mod3$y)))
names(bank_impute)[length(names(bank_impute))] <- "y"
str(bank_impute)

#bank_impute <- centralImputation(bank_mod3) ## Single Imputation (범주형: 최빈값 // 연속형: 평균)


write.csv(bank_impute, "C:\\Users\\Hyunjin\\Dropbox\\2017년 1학기\\고급 데이터마이닝\\Data\\bank-additional\\bank_impute.csv", row.names=F, na="")
## 최종 데이터 



## (예시)
## Decision Tree  
id.train <- sample(1:nrow(bank_impute), nrow(bank_impute)*0.8)


bank_tree <- rpart(y~., data=bank_impute[id.train,], method="class", parms=list(prior=c(1,1)/2))
plot(bank_tree)
text(bank_tree, pretty=0)

printcp(bank_tree)
plotcp(bank_tree)


bank_ptree<-prune(bank_tree, cp= bank_tree$cptable[which.min(bank_tree$cptable[,"xerror"]),"CP"])
plot(bank_ptree)
text(bank_ptree)


pred <- predict(bank_ptree, bank_impute[-id.train,], type="class")
mc <- table( bank_impute$y[-id.train], pred)
confusionMatrix( bank_impute$y[-id.train], pred)
print(mc)
err.resub <- 1-(mc[1,1]+mc[2,2])/sum(mc)
print(err.resub)


