pkgs <- c("plyr", "DJL", "e1071", "ROCR",
          "caret", "MASS", "rpart", "rattle", "adabag","randomForest", "caretEnsemble","kernlab", "mice", "DMwR")
tt <- sapply(pkgs, require, character.only=T)

## ��Ű�� ��ġ 
for(i in 1:length(tt)) {
  if(tt[i]==FALSE) {
    install.packages(names(tt)[i])
  }
}

## ��Ű�� ���� 
sapply(pkgs, require, character.only=T)

## ������ �Է� 
## ������ unknown�� �̸� �������� ���� 
bank <- read.csv("C:\\Users\\Hyunjin\\Dropbox\\2017�� 1�б�\\��� �����͸��̴�\\Data\\bank-additional\\bank-additional-full2.csv", header=T, stringsAsFactors=F)

## Yes, No ���� 
table(bank$y)

## �׷��� 
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

## ������ ������ ã�ƺ��� 
summary(glm(as.factor(loan)~as.factor(housing), data=bank, family="binomial")) ## ���� ������� ���� // ���� �� ���� ���� �ϳ��� ��� 


## pass day ���� 
hist(subset(bank$pdays, bank$pdays!=999), main="pdays", xlab="�������", ylab="��")
summary(subset(bank$pdays, bank$pdays!=999))

bank$pdays_cat <- ifelse(bank$pdays==999, 0, 
                         ifelse(bank$pdays<=5, 1,
                                ifelse(bank$pdays<=10, 2,
                                       ifelse(bank$pdays<=15, 3, 4))))

## Job ���� ��� by �������� 
bank$job <- ifelse(bank$job=="admin." | bank$job=="entrepreneur" | bank$job=="management" | bank$job=="services", "management", bank$job)

## Education ���� ��� by �������� 
bank$education <- ifelse(bank$education=="basic.4y" | bank$education=="basic.6y" | bank$education=="basic.9y" | bank$education=="illiterate" | bank$education=="high.school", "le_hs", 
                         ifelse(bank$education=="professional.course" | bank$education=="university.degree", "ge_univ", bank$education))

str(bank)


bank_mod <- bank[,-which(names(bank) %in% c("month", "day_of_week", "pdays", "duration"))] ## month, day_of_week�� �λ���Ʈ�� ����� ����� ���� 
bank_mod$job <- ifelse(bank_mod$job=="", NA, bank_mod$job) ## ������ NA�� �ν��� �� �ؼ� ����
bank_mod$marital <- ifelse(bank_mod$marital=="", NA, bank_mod$marital)
bank_mod$education <- ifelse(bank_mod$education=="", NA, bank_mod$education)
head(bank_mod)

write.csv(bank_mod, "C:\\Users\\Hyunjin\\Dropbox\\2017�� 1�б�\\��� �����͸��̴�\\Data\\bank-additional\\bank-additional-full3.csv", row.names=F, na="")
## ������ 

## ����ġ ������ ������ ������ ���� 
bank_mod2 <- bank_mod[!(rowSums( is.na( bank_mod[,which(names(bank_mod) %in% c("default", "housing", "loan"))] ) )==3),] ## default�� �� ���������� ������ ���谡 ���� �� ������, �̰� 3���� ���� ������ �� �������� 
nrow(bank_mod2)
mean(!complete.cases(bank_mod2)) ## ����ġ �� ����� 25.57% ������ ���� 

miss1 <- as.data.frame(abs(is.na(bank_mod2)))
miss2 <- miss1[which(apply(miss1,2,sum)>0)]

cor(miss2) ## ������ �ִ� �������� �������� dummy ������ ������� 

write.csv(cor(miss2), "C:\\Users\\Hyunjin\\Dropbox\\2017�� 1�б�\\��� �����͸��̴�\\Data\\bank-additional\\miss2_cor.csv", row.names=T, na="")
## ������ 

miss3 <- cor(bank_mod2[,-c(2,3,4,5,6,7,8,11,17)], miss2, use="pairwise.complete.obs") ## ������ �ִ� �������� �������� dummy ������ ������ ���� ���� ������� 

write.csv(miss3, "C:\\Users\\Hyunjin\\Dropbox\\2017�� 1�б�\\��� �����͸��̴�\\Data\\bank-additional\\miss3_cor.csv", row.names=T, na="")
## ������ 

## �� ���� ���� ��� 
bank_mod3 <- bank_mod2[,-which(names(bank_mod2) %in% "housing")]
str(bank_mod3)

## ĳ���� (�� �ϸ� Imputation ����� �� ��)
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

#bank_impute <- centralImputation(bank_mod3) ## Single Imputation (������: �ֺ� // ������: ���)


write.csv(bank_impute, "C:\\Users\\Hyunjin\\Dropbox\\2017�� 1�б�\\��� �����͸��̴�\\Data\\bank-additional\\bank_impute.csv", row.names=F, na="")
## ���� ������ 



## (����)
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


