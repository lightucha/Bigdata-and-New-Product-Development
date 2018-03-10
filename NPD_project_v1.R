#####################################################################################
### Setting up Environment
#####################################################################################
library(foreach)
library(plyr)
library(stringr)
library(tm)

# read data
tag_shinchon<-read.csv("tagg.csv",header=T,stringsAsFactors = F)
check_foreach <- foreach(i=3:nrow(tag_shinchon), .combine = c) %do% {
  if(tag_shinchon[i,] == tag_shinchon[i-1,] | tag_shinchon[i,] == tag_shinchon[i-2,]){
    print(i)
  }
}
tag_shinchon_clean <- data.frame(tag_shinchon[-check_foreach,])

###########################################
####### extract tag
###########################################
#### finding #,','
gregexpr("#", tag_shinchon_clean[1,1])
gregexpr(",", tag_shinchon_clean[1,1])

a <- foreach(i=1:nrow(tag_shinchon_clean), .combine = c) %do% {
  print( gregexpr("#", tag_shinchon_clean[i ,1]))
}
b <- foreach(i=1:nrow(tag_shinchon_clean), .combine = c) %do% {
  print( gregexpr(",", tag_shinchon_clean[i ,1]))
}

# extract tag
tag<-foreach(i=1:NROW(tag_shinchon_clean)) %do% {
  
  foreach(j = 1:length(a[[i]])) %do% {
    
    if (is.na(a[[i]][j+1])==FALSE){
      
      substr(tag_shinchon_clean[i,1],a[[i]][j],a[[i]][j+1]-1)}
    
    else if(is.na(a[[1]][j+1])==TRUE){
      
      substr(tag_shinchon_clean[i,1],a[[i]][j],b[[i]][1]-1)
    }}
}

# cleaning tag
tag<-foreach(i=1:NROW(tag_shinchon_clean)) %do% {
  foreach(j = 1:length(a[[i]])) %do% {
    word(tag[[i]][j],1)
  }
}

tag <- gsub(x=tag, "[^[:alnum:]]", " ") # 함수가 사라졌으므로 gsub로 수정 
tag <- gsub(x=tag, "(\\s+|ㅋ+|ㅜ+|ㅠ+|ㅎ+)", " ") # 함수가 사라졌으므로 gsub로 수정
tag <- gsub(x=tag, "\\d+", "") ## 숫자제거 (추가)
tag <- gsub(x=tag, "\\.+", "") ## 점제거 (추가)
tag <- gsub(x=tag, "\\#+", "") ## 해시태그 제거 (추가)
tag <- gsub(x=tag, "\\ㅡ+", "") ## 대시 제거 (추가)
tag <- gsub(x=tag, "\\^+", "") ## 써컴플렉스 제거 (추가)
tag <- gsub(x=tag, "\\|+", "") ## 버티컬 바 제거 (추가)
tag <- toupper(tag) ## 대문자화 
tag <- gsub("[A-Za-z]", "", tag)
trailing_space <- "[ ]{2,}|[ ]+$"
tag <- gsub(pattern = trailing_space, replacement = "", tag)
tag <- gsub("チャ シュ", "", tag)
tag <- gsub("ラ メン", "", tag)
tag <- gsub("麻辣", "", tag)
tag <- str_split(tag," ")

# 리스트 최대 길이 찾기
check_len <- list()
for (i in 1:length(tag)){
  check_len[i] <- length(tag[[i]])
}
max_len <- max(unlist(check_len))

# 리스트 길이 맞추기
for (i in 1:length(tag)){
  
  if( length(tag[[i]]) < max_len){
    
    num <- length(tag[[i]])
  }
  
  for (j in num:max_len){
    
    tag[[i]][j] <- "NA"
    
  }
  print(i)
  print(j)
  print(num)
}

ntag <- length(unlist(tag))
spltag <- unlist(tag)

temp <- matrix(data = spltag, nrow = max_len, ncol = ntag/max_len)
temp<-as.data.frame(temp)
#################################
## test
#################################
# ad dictionary
bad<-read.csv("bad.csv")
ad<-as.vector((t(bad)))
ad<-str_trim(ad)
ad

# cleaning nogada
length(intersect(ad,str_trim(t(tag[[i]]))))
length(intersect(ad,str_trim(t(tag[[i]]))))/length(str_trim(t(tag[[i]])))

# find out ad
result<-foreach(i= 1:length(tag), .combine=c) %do% {
  length(intersect(ad,str_trim(tag[[i]])))/length(str_trim(tag[[i]]))
}
result<-round(result,2)
result

# 결과 병합
temp<-cbind(t(temp),result)
