#Text cleansing

ExtractWordFreq <- function (temp) {
  filelist <- NULL
  for (i in 1:length(temp)) {
    filelist[[i]] <- readLines((file(temp[i], encoding = "UCS-2LE")))
  }
  
  wordtxtlines <- unlist(filelist)
  wordtxtlines <- as.character(na.omit(wordtxtlines))
  
  useSejongDic() # 여기서 오류가 날 경우 RSQLite 패키지 설치 후 코드 재실행
  
  
  wordtxtlines <- gsub(x=wordtxtlines, "[^[:alnum:]]", " ") # 함수가 사라졌으므로 gsub로 수정
  wordtxtlines <- gsub(x=wordtxtlines, "(\\s+|ㅋ+|ㅜ+|ㅠ+|ㅎ+)", " ") # 함수가 사라졌으므로 gsub로 수정
  wordtxtlines <- gsub(x=wordtxtlines, "\\d+", "") ## 숫자제거 (추가)
  wordtxtlines <- gsub(x=wordtxtlines, "\\.+", "") ## 점제거 (추가)
  wordtxtlines <- gsub(x=wordtxtlines, "\\#+", "") ## 해시태그 제거 (추가)
  wordtxtlines <- gsub(x=wordtxtlines, "\\ㅡ+", "") ## 대시 제거 (추가)
  wordtxtlines <- gsub(x=wordtxtlines, "\\^+", "") ## 써컴플렉스 제거 (추가)
  wordtxtlines <- gsub(x=wordtxtlines, "\\|+", "") ## 버티컬 바 제거 (추가)
  wordtxtlines <- toupper(wordtxtlines) ## 대문자화 
  # NROW(wordtxtlines)
  
  # Line별로 명사형 찾아내기 -> List, 형태소를 추출하는 부분에 시간이 많이 소요됨
  # USE.NAMES = F 원래 문장이 필요 없으므로
  #nounslist <- sapply(wordtxtlines, extractNoun, USE.NAMES = F)
  nounslist <- alply(wordtxtlines, 1, extractNoun, .inform=F)
  
  
  
  # 전체 형태소 만들기 -> vector
  nouns <- unlist(nounslist)
  
  # 없애고 싶은 단어 제거하기 -> deleteword.txt
  #dwf <- file(deletewordfile, blocking=F, open='r')
  #deletewords <- readLines(dwf)
  #close(dwf)
  #for (deleteword in deletewords) nouns <- gsub(deleteword, "", nouns)
  
  # 형태소 중 2글자 이상인것들만 가져옴
  realnouns <- nouns[nchar(nouns) >= 2]
  
  # 형태소별 count -> table만드는 부분에 시간이 많이 소요됨
  wordcount <- sort(table(realnouns), decreasing=T)
  
  return(wordcount)
}