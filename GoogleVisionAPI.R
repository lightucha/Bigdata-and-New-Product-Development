##=======================================================================================
## 01. 환경설정
##=======================================================================================
setwd("C:/Users/Chris/Documents/practice/NPD")

library(magrittr)
#library(formattable)
library(jsonlite)

##=======================================================================================
## 02. 구글 비젼 API 호출 함수
##=======================================================================================

getResult <- function(f, type = "TEXT_DETECTION"){
  library("httr")
  library("base64enc")
  CLOUD_VISION_KEY <- "AIzaSyD24ibDfvoNi3fR8dKRvn8MFLAy8EmUAcs" 
  u <- paste0("https://vision.googleapis.com/v1/images:annotate?key=", CLOUD_VISION_KEY)
  img <- readBin(f, "raw", file.info(f)[1, "size"])
  base64_encoded <- base64encode(img)
  body <- list(requests = list(image = list(content = base64_encoded),
                               features = list(type = type,
                                               maxResults = 10))
  )
  
  res <- POST(url = u,
              encode = "json",
              body = body,
              content_type_json())
}

##=======================================================================================
## 03. 구글 비젼 API 활용
##=======================================================================================
# 3.1. 사진 속 물체 탐지
getResult("dopeach.png", "LABEL_DETECTION") %>% 
  content(as = "text") %>% fromJSON() %$% 
  responses$labelAnnotations %>% as.data.frame()

# 3.2. 텍스트 탐지
getResult("335.jpg", "TEXT_DETECTION") %>% 
  content(as = "text") %>% fromJSON() %$% 
  responses$textAnnotations[[1]]$description[[1]]

# 3.3. 주요지명 탐지
getResult("경복궁.jpg", "LANDMARK_DETECTION") %>% 
  content(as = "text") %>% fromJSON() %$% 
  responses$landmarkAnnotations

# 3.4. 로고 탐지
getResult("명품로고.jpg", "LOGO_DETECTION") %>% 
  content(as = "text") %>% fromJSON() %$% 
  responses$logoAnnotations