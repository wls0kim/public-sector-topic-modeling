# =============================================================================
# 공무원 온라인 리뷰 전처리: 형태소 분석, 동의어 처리, 말뭉치 생성
#
# - 입력: blind_raw.csv (리뷰 원자료, CP949), synonymDic.csv (동의어 사전)
# - 출력: data0605.csv (날짜 형식을 정리한 원자료), userDIC.txt (형태소 분석용 사용자 사전)
# - 형태소 분석에 NLP4kec 패키지를 씁니다 (CRAN에 없어 GitHub에서 설치).
# - 최종 STM 모형과 해석은 02_stm_pros_cons.Rmd에 있습니다. 이 스크립트에서 시험했던
#   토픽 수 탐색과 시간 추이 모형(주, 월 단위 공변량)은 최종 분석에 쓰지 않아 정리하며 뺐습니다.
# =============================================================================

library(dplyr)
library(stringr)
library(lubridate)  # 날짜 처리
library(tidytext)   # unnest_tokens
library(NLP4kec)    # 한국어 형태소 분석
library(stm)        # textProcessor, prepDocuments

# 작업 폴더를 이 스크립트가 있는 폴더로 지정해 실행

# 1. 자료 불러오기 -------------------------------------------------------------
data <- read.csv("blind_raw.csv", fileEncoding = "cp949")

# 날짜 앞에 "20"을 붙여 연도를 네 자리로 만든다 (예: 21-11-03 → 2021-11-03)
data$date.tmp <- paste0("20", data$date.tmp)
colnames(data) <- c("title", "pros", "cons", "total", "career", "wlb", "compensation",
                    "culture", "seniormanagement", "status", "date")

# 한글이 깨지지 않도록 CP949로 저장
write.csv(data, "data0605.csv", row.names = FALSE, fileEncoding = "CP949")
data <- read.csv("data0605.csv", fileEncoding = "cp949")
data$date <- as.Date(data$date)  # "2021-11-03" 형식 문자열을 날짜로 변환


# 2. 문장부호와 띄어쓰기 제거 ---------------------------------------------------
# 원문은 남겨 두고 별도 열(Conspos, Prospos)에 정제한 텍스트를 만든다. "MZ세대"처럼 영문이 섞인 단어가 있어 영문은 지우지 않는다.

data$Conspos <- data$cons %>%  # 원문은 남겨두기 위해서 따로 열을 만들어야 함   
  str_replace_all("[[:punct:]]"," ")%>%   # 모든 문장부호 제거 
  str_squish() %>%  #모든 공백을 단일 스페이스로 처리 
  str_remove_all(pattern = '\\s+')

data$Prospos <- data$pros %>%  
  str_replace_all("[[:punct:]]"," ")%>%  
  str_squish() %>%  
  str_remove_all(pattern = '\\s+')

# 3. 사용자 사전 ---------------------------------------------------------------
# 형태소 분석기가 쪼개면 안 되는 신조어, 직장 용어를 한 단어로 등록한다.
userWord <- c("고인물", "부바부","부서바이부서","부바이부",
              "팀바이팀","팀바팀","워라밸","워라벨",
              "사람바이사람","사바사","칼퇴","교육체계",
              "조직체계","꼰대문화","mz세대","MZ세대",
              "Mz세대","가스라이팅","네임벨류","내임벨류",
              "네임밸류","내임밸류","자기계발","자기개발",
              "자기계발비","자기개발비","눈치보다","신경쓰다",
              "칼퇴근","출퇴근","퇴근시간","출근시간","군대문화",
              "연차사용","유연근무제","실무진","역피라미드","플젝",
              "케바케","케이스바이케이스","역세권","노답","비추천",
              "워크라이프","워크라이프밸런스","워크앤라이프밸런스",
              "엘베","엘배","앨배","워커홀릭","하루종일","레드오션",
              "블루오션","롤","성과급","HR","hr","철밥통","현타","갑질",
              "또라이","세금도둑","저놈","전직장","업무량","업무양",
              "실수령액","업무강도"
)

write.table(
  x = userWord,
  quote = TRUE,
  file = './userDIC.txt',
  row.names = FALSE,
  col.names = FALSE,
  fileEncoding = "UTF-8")


r_parser_r("세금도둑이다.", language = "ko", korDicPath = "userDIC.txt")  # 사용자 사전 적용 확인

# 4. 형태소 분석과 토큰화 --------------------------------------------------------
fun1 <- function(x){
  r_parser_r(contentVector = x, language = "ko", korDicPath = "userDIC.txt") %>%  
    as.list()
}

#토큰화 
data <- tidytext::unnest_tokens(
  tbl = data,      
  input = Conspos,   
  output = Conspos,   
  token =  fun1    
  
)

data <- tidytext::unnest_tokens(
  tbl = data,      
  input = Prospos,   
  output = Prospos,   
  token =  fun1    
  
)

# 형태소 분석 결과(Conspos, Prospos 열 포함)를 저장한다. 02_stm_pros_cons.Rmd가 이 파일을 읽는다.
write.csv(data, "blind.csv", row.names = FALSE, fileEncoding = "CP949")


# 5. 동의어 처리 ----------------------------------------------------------------
# 표기만 다른 같은 단어를 하나로 통일 (예: 워라벨 → 워라밸)
synonymDic <- read.csv("synonymDic.csv", header = T, fileEncoding = "CP949")

synonym_processing <- function(parsedVector, synonymDic) {
  for (i in 1:nrow(synonymDic)) {
    parsedVector <- gsub(synonymDic$originWord[i], synonymDic$changeWord[i], parsedVector)
  }
  parsedVector
}

parsedDatacons = synonym_processing(parsedVector = data$Conspos, synonymDic = synonymDic)
parsedDatapros = synonym_processing(parsedVector = data$Prospos, synonymDic = synonymDic)

data$Conspos<-parsedDatacons
data$Prospos<-parsedDatapros 


# 6. 장점, 단점 자료 분리와 날짜 변수 ------------------------------------------------
datapros <- data[, c(-3, -10, -12)]  # cons, status, Conspos 제거
datacons <- data[, c(-2, -10, -13)]  # pros, status, Prospos 제거

datacons <- na.omit(datacons)
datapros <- na.omit(datapros)

datacons <- datacons %>%  mutate(day = as.numeric(datacons$date)-18932)
datacons <- datacons %>%  mutate(week = ceiling(day / 7)) 
datacons <- datacons %>%  mutate(month = month(ymd(date)))
datacons <- datacons %>% mutate(
  month = ifelse(
    year(date) == 2021,
    month(date)-10,
    ifelse(
      year(date) == 2022,
      month(date) + 2,
      month(date) + 14
    )
  )
)

datapros <- datapros %>%  mutate(day = as.numeric(datapros$date)-18932)
datapros <- datapros %>%  mutate(week = ceiling(day / 7)) 
datapros <- datapros %>% mutate(
  month = ifelse(
    year(date) == 2021,
    month(date)-10,
    ifelse(
      year(date) == 2022,
      month(date) + 2,
      month(date) + 14
    )
  )
)

# 7. 불용어 제거와 말뭉치 생성 -----------------------------------------------------
stopwords_ko <- c("하다", "있다", "되다", "회사", "단점", "장점", "가량", "가지", "각", "수",
                  "곳", "뿐", "등", "만", "시", "등", "때", "나", "듯", "간", "편", "외",
                  "년", "차", "것", "날", "분", "중", "줄", "간", "내", "뭣", "급", "점",
                  "원", "데")

processedcons <- textProcessor(documents = datacons$Conspos, metadata = datacons,
                               wordLengths = c(1, Inf), customstopwords = stopwords_ko)
processedpros <- textProcessor(documents = datapros$Prospos, metadata = datapros,
                               wordLengths = c(1, Inf), customstopwords = stopwords_ko)


outcons <- prepDocuments(processedcons$documents, processedcons$vocab,
                         processedcons$meta, lower.thresh = 5)
outpros <- prepDocuments(processedpros$documents, processedpros$vocab,
                         processedpros$meta, lower.thresh = 5)


# 8. STM 입력 확인 --------------------------------------------------------------

outdocspros<- outpros$documents
outvocabpros<- outpros$vocab
outmetapros <- outpros$meta
summary(outpros)

outdocscons<- outcons$documents
outvocabcons <- outcons$vocab
outmetacons <- outcons$meta
summary(outcons)
