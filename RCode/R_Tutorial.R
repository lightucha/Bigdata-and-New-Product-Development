#2017.04.10
#NPD
#R tutorial by Jung, hyunjin

sapply(c("readxl", "ggplot2"), require, character.only=T)
lapply(c("readxl", "ggplot2"), require, character.only=T)


x <- c(52, 23, 57, 68, 89, 99, 110, 1, 3, 11, 31, 36, 27, 22, 40)

j=k=1 
even = odd = NULL

for(i in 1:length(x)){
  if(x[i]%%2==0){
    even[j] = x[i]
    j = j+1
  }
  else{
    odd[k] = x[i]
    k=k+1
  }
  print(even); print(odd)
}


getwd()
setwd("C:/Users/Chris/Documents/practice/R_Tuorial_Data")
library(xlsx)
Seoul_climate <- read.xlsx("2016년 11월 서울특별시 일평균 기상자료.xlsx",1)


colnames(Seoul_climate) <- c("ID", "City", "Date", "Temp", "Rainfall", "Humid")

str(Seoul_climate)

#Sys.setlocale("LC_COLLATE", "ko_KR.UTF-8")

#install.packages("extrafont")
#library(extrafont)
#font_import() #시스템 폰트 import


