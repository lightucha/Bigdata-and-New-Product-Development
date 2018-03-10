######################################################################################
tag_shinchon <- read.csv("instatext.csv", header = TRUE, stringsAsFactors = FALSE)

pkgs <- c("rvest", "dplyr", "tm", "SnowballC", "ggplot2", "wordcloud")
sapply(pkgs, require, character.only = T)


library(parallel)
library(foreach)


check_foreach <- foreach(i=3:nrow(tag_shinchon), .combine = c) %do% {
  if(tag_shinchon[i,] == tag_shinchon[i-1,] | tag_shinchon[i,] == tag_shinchon[i-2,]){
    print(i)
  }
}

tag_shinchon_clean <- data.frame(tag_shinchon[-check_foreach,])

colnames(tag_shinchon_clean) <- "TEXT"


temp <- gregexpr("#", tag_shinchon_clean[1,])
tempTag <- as.vector(temp[[1]])

temp2 <- gregexpr(",", tag_shinchon_clean[1,])
tempcomma <- as.vector(temp2[[1]])

substr(tag_shinchon_clean[1,], tempTag[1], tempTag[2]-1)

sampleTag <- foreach(i=1:length(tempTag), .combine = c) %do% {
  if(is.na(tempTag[i+1])==TRUE){
    substr(tag_shinchon_clean[1,], tempTag[i], tempcomma[length(tempcomma)]-1)
  }
  else{
    substr(tag_shinchon_clean[1,], tempTag[i], tempTag[i+1]-1)
  }
}

######### 여러 문장(1개 이상의 문장)에 대해서 패턴에 맞는 부분만 추출한다.#############
extractPattern <- function(pattern, sentences){
  
  extractFunction <- function(x){
    
    patternloc <- gregexpr(pattern, x)
    
    patternstart <- as.vector(patternloc[[1]])
    
    searchlen <- attr(patternloc[[1]],"match.length")
    
    if(patternstart[1] != -1){
      
      result <- mapply(function(s,l){substr(x, s, s+l-1)}, patternstart, searchlen)
    }else{
      
      result = NA
    }
    
    return(result)
  }
  
  lapply(sentences, extractFunction)
}
#######################################################################################


###############2nd Version###################################################################
extractPattern("#", tag_shinchon_clean[1,])

library(stringr)
str_extract_all(tag_shinchon_clean[1,],"#(\\w)(\\w)(\\w)(\\w)(\\w)(\\w)(\\w)(\\w)(\\w)(\\w)(\\w)")


extractTag <- function(pattern, sentences){
  
  extractTagFunction <- function(x){
    
    Tagloc <- gregexpr(pattern, x)
    
    Tagstart <- as.vector(Tagloc[[1]])
    
    Tagsearchlen <- attr(Tagloc[[1]],"match.length")
    
    foreach(i= 1:length(Tagstart), .combine = c) %do% {
      
      result <- mapply(substr(x, Tagstart[i], Tagstart[i+1]-1))
      #result <- mapply(function(s,l){substr(x, s, l-1)}, Tagstart[i], Tagstart[i+1])
    }
    
    return(result)
  }
  
  lapply(sentences, extractTagFunction)
}

extractTag("#", tag_shinchon_clean[i,])
########################################################################################


library(stringr)
str_trim(sampleTag)

shinchon_Tag <- list()
for (j in 1:nrow(tag_shinchon_clean)){
  
  #print(j)
  temp <- gregexpr("#", tag_shinchon_clean[j,])
  tempTag <- as.vector(temp[[1]])
  #print(tempTag)
  
  temp2 <- gregexpr(",", tag_shinchon_clean[j,])
  tempcomma <- as.vector(temp2[[1]])
  #print(tempcomma)
  
  
  shinchon_Tag[j] <- foreach(i=1:length(tempTag), .combine = c) %do% {
    if(length(tempTag) %% 2 != 0){
      substr(tag_shinchon_clean[j,], tempTag[i], tempTag[i+1]-1)
      }
    else{
      substr(tag_shinchon_clean[j,], tempTag[i], tempcomma[length(tempcomma)]-1)
     }
  }
  print(i)
  print(j)
}

shinchon_Tag
#################################################################################################3




###################################by jang###################################################
# read data
tag_shinchon<-read.csv("instatext.csv",header=T,stringsAsFactors = F)
check_foreach <- foreach(i=3:nrow(tag_shinchon), .combine = c) %do% {
  if(tag_shinchon[i,] == tag_shinchon[i-1,] | tag_shinchon[i,] == tag_shinchon[i-2,]){
    print(i)
  }
}
tag_shinchon_clean <- data.frame(tag_shinchon[-check_foreach,])


i=1
bb<-for(i in NROW(tag_shinchon_clean)){
  gregexpr("#", tag_shinchon_clean[i ,1])
}
bb


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
      
      (substr(tag_shinchon_clean[i,1],a[[i]][j],a[[i]][j+1]-1))
      
    }
    
    else if(is.na(a[[1]][j+1])==TRUE){
      
      (substr(tag_shinchon_clean[i,1],a[[i]][j],b[[i]][1]-1))
    }}
}

# cleaning tag
foreach( )

tag[[175]]
word(tag[[100]][3],1)

tag1<-foreach(i=1:NROW(tag_shinchon_clean)) %do% {
  foreach(j = 1:length(a[[i]])) %do% {
    word(tag[[i]][j],1)
  }
}

######################################################################################

shinchon_Tag[145] <- gsub("#(\\w)*","",shinchon_Tag[145][1], fixed = TRUE)
grep("#(\\w)*",shinchon_Tag[145][1])
shinchon_Tag[145]

for (i in 1:nrow(tag_shinchon_clean)){
  shinchon_Tag[i] <- gsub("\\d+","",shinchon_Tag[i][1])
  shinchon_Tag[i] <- gsub("\\)","",shinchon_Tag[i][1])
  shinchon_Tag[i] <- gsub("\\(","",shinchon_Tag[i][1])
  shinchon_Tag[i] <- gsub("\\n","",shinchon_Tag[i][1], fixed = TRUE)
  shinchon_Tag[i] <- gsub("@[\\w]*","",shinchon_Tag[i][1])
  shinchon_Tag[i] <- gsub("#[a-zA-Z]+","",shinchon_Tag[i][1])
  #shinchon_Tag[i] <- gsub("#[[:alpha:]]+","",shinchon_Tag[i][1])
 
}

shinchon_Tag




library(gsubfn)
strapplyc(shinchon_Tag[174], "#(.*)", simplify = TRUE)

sub(" .*", "", shinchon_Tag[174][1])


######################################################################################################

pkgs <- c("rvest", "dplyr", "tm", "SnowballC", "ggplot2", "wordcloud")
sapply(pkgs, require, character.only = T)

reviews_dtm <- function(reviews) {
  
  # First remove all unusual characters
  reviews <- gsub("[^[:graph:]]", " ", reviews)
  
  # Create corpus of reviews
  rcorpus <- Corpus(VectorSource(reviews))
  
  # Clean text in corpus
  rcorpus <- tm_map(rcorpus, removePunctuation)
  rcorpus <- tm_map(rcorpus, tolower)
  rcorpus <- tm_map(rcorpus, removeNumbers)
  rcorpus <- tm_map(rcorpus, removeWords, stopwords('english'))
  rcorpus <- tm_map(rcorpus, removeWords, c("airbnb")) # manual stopwords
  
  # Stem words, e.g. locked and locking become lock
  rcorpus <- tm_map(rcorpus, stemDocument) 
  
  # Strip unnecessary whitespaces
  rcorpus <- tm_map(rcorpus, stripWhitespace) 
  
  # convert to document term matrix
  dtm <- DocumentTermMatrix(rcorpus)
  
  return(dtm)
}

tag_nogada <- read.csv("nogada.csv", header=TRUE)
a <- reviews_dtm(tag_nogada)
inspect(a)
a

tag_nogada <- gsub("[^[:graph:]]", " ", tag_nogada)
rcorpus <- Corpus(VectorSource(tag_nogada))
rcorpus <- tm_map(rcorpus, removePunctuation)
rcorpus <- tm_map(rcorpus, stemDocument)
rcorpus <- tm_map(rcorpus, stripWhitespace) 
dtm <- DocumentTermMatrix(rcorpus)



#####################################################################################
### Setting up Environment

library(foreach)
library(plyr)
library(stringr)
library(tm)

# read data
tag_shinchon<-read.csv("instatext.csv",header=T,stringsAsFactors = F)
check_foreach <- foreach(i=3:nrow(tag_shinchon), .combine = c) %do% {
  if(tag_shinchon[i,] == tag_shinchon[i-1,] | tag_shinchon[i,] == tag_shinchon[i-2,]){
    print(i)
  }
}
tag_shinchon_clean <- data.frame(tag_shinchon[-check_foreach,])


### extract tag

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
tag1<-foreach(i=1:NROW(tag_shinchon_clean)) %do% {
  foreach(j = 1:length(a[[i]])) %do% {
    word(tag[[i]][j],1)
  }
}


tag1 <- str_trim(tag1)
tag1 <- gsub("[^[:graph:]]", " ", tag1)
tag1 <- gsub("\\s", "", tag1)
tag1 <- gsub("\\n", "", tag1)

##################data cleansing#######################################################
tag1 <- gsub(x=tag1, "[^[:alnum:]]", " ") # 함수가 사라졌으므로 gsub로 수정 / 숫자, 문자 빼고 다 지우
tag1 <- gsub(x=tag1, "(\\s+|ㅋ+|ㅜ+|ㅠ+|ㅎ+)", " ") # 함수가 사라졌으므로 gsub로 수정
tag1 <- gsub(x=tag1, "\\d+", "") ## 숫자제거 (추가)
tag1 <- gsub(x=tag1, "\\.+", "") ## 점제거 (추가)
tag1 <- gsub(x=tag1, "\\#+", "") ## 해시태그 제거 (추가)
tag1 <- gsub(x=tag1, "\\ㅡ+", "") ## 대시 제거 (추가)
tag1 <- gsub(x=tag1, "\\^+", "") ## 써컴플렉스 제거 (추가)
tag1 <- gsub(x=tag1, "\\|+", "") ## 버티컬 바 제거 (추가)
tag1 <- toupper(tag1) ## 대문자화 
tag1 <- gsub("[A-Za-z]", "", tag1)
trailing_space <- "[ ]{2,}|[ ]+$"
tag1 <- gsub(pattern = trailing_space, replacement = "", tag1)
tag1 <- gsub("チャ シュ", "", tag1)
tag1 <- gsub("ラ メン", "", tag1)
tag1 <- gsub("麻辣", "", tag1)
###########################################################################################################

# list in list to data.frame (tag)
dd  <-  as.data.frame(matrix(unlist(tag1), nrow=length(unlist(tag1[1]))))
ddd<-t(dd)



##########
## test


no<-read.csv("nogada.csv",header=T)
bad<-read.csv("bad.csv")

# ad dictionary
ad<-as.vector((t(bad)))
ad<-str_trim(ad)
ad


# find out ad
for(i in 1:NROW(no)){
  
  print(length(intersect(ad,str_trim(t(no[i,])))))
  
}
