# =============================================================================
# 석사학위논문(2024.02) 분석: 공무원 리뷰의 장점, 단점, 한줄평 토픽 모델링
#
# - 입력: raw.csv (리뷰 원자료), synonymDic.csv (동의어 사전)
# - 공변량 없이 장점(K = 23), 단점(K = 34) 모형을 적합하고 대표 단어, 대표 문서를 정리한다.
#   만족도를 공변량으로 넣은 학술지 버전은 02_stm_pros_cons.Rmd에 있다.
# - 형태소 분석에 NLP4kec 패키지를 쓴다 (CRAN에 없어 GitHub에서 설치).
# - 순서: 텍스트 정제 → 형태소 분석 → 동의어 통일 → 불용어 제거와 말뭉치 생성
#         → 토픽 수 탐색 → STM 적합 → 대표 단어, 대표 문서 저장 → 토픽 비율 그래프
# =============================================================================

library(tm)        # 텍스트 처리
library(stringr)   # 문자열 처리
library(NLP4kec)   # 한국어 형태소 분석
library(openxlsx)  # 엑셀 쓰기
library(dplyr)     # 자료 가공
library(stm)       # 구조적 토픽 모델링
library(readr)     # csv 읽기

# 작업 폴더를 이 스크립트가 있는 폴더로 지정해 실행


# 1. 자료 불러오기 -------------------------------------------------------------
data <- read_csv("raw.csv")


# 2. 문장부호와 띄어쓰기 제거 ---------------------------------------------------
# 원문 열(cons, pros, title)은 남겨 두고 정제한 텍스트를 새 열(*pos)에 만든다.
clean_text <- function(x) {
  x %>%
    str_replace_all("[[:punct:]]", " ") %>%  # 문장부호를 공백으로
    str_squish() %>%                         # 연속 공백을 하나로
    str_remove_all("\\s+")                   # 띄어쓰기를 모두 지움 (형태소 분석기가 다시 나눔)
}

data$Conspos  <- clean_text(data$cons)   # 단점
data$Prospos  <- clean_text(data$pros)   # 장점
data$titlepos <- clean_text(data$title)  # 한줄평


# 3. 사용자 사전과 형태소 분석 ---------------------------------------------------
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
              "실수령액","업무강도")

write.table(userWord, file = "./userDIC.txt", quote = TRUE,
            row.names = FALSE, col.names = FALSE, fileEncoding = "UTF-8")

# 문서마다 형태소 분석 결과(공백으로 구분된 형태소 문자열)를 돌려준다
fun1 <- function(x) {
  r_parser_r(contentVector = x, language = "ko", korDicPath = "./userDIC.txt") %>%
    as.list()
}

for (col in c("Conspos", "Prospos", "titlepos")) {
  data <- tidytext::unnest_tokens(tbl = data, input = !!col, output = !!col, token = fun1)
}


# 4. 동의어 처리 ----------------------------------------------------------------
# 표기만 다른 같은 단어를 하나로 통일한다 (예: 워라벨 → 워라밸)
synonymDic <- read.csv("./synonymDic.csv", header = TRUE, fileEncoding = "CP949")

synonym_processing <- function(parsedVector, synonymDic) {
  for (i in seq_len(nrow(synonymDic))) {
    parsedVector <- gsub(synonymDic$originWord[i], synonymDic$changeWord[i], parsedVector)
  }
  parsedVector
}

data$Conspos  <- synonym_processing(data$Conspos, synonymDic)
data$Prospos  <- synonym_processing(data$Prospos, synonymDic)
data$titlepos <- synonym_processing(data$titlepos, synonymDic)


# 5. 불용어 제거와 말뭉치 생성 ---------------------------------------------------
# 한국어 불용어 목록에 "회사", "장점", "단점"처럼 모든 리뷰에 나오는 단어를 더했다
stopwords_ko <- c("가량", "가지", "각", "간", "갖은", "개", "개국", "개년", "개소", 
          "개월", "걔", "거", "거기", "거리", "건", "것", "겨를", 
          "격","겸", "고", "군", "군데", "권", "그", "그거", "그것", "그곳", 
          "그까짓", "그네", "그녀", "그놈", "그대", "그래", "그래도","그서","그러나"
          ,"그러니","그러니까","그러다가","그러면","그러면서","그러므로"
          ,"그러자","그런","그런데", "그럼", "그렇지만", "그루", 
          "그리고","그리하여", "그분", "그이","그쪽","근", "근데", "글쎄",
          "글쎄요","기","김", "나", "나름", "나위", "남짓", "내", "냥", 
          "너", "너희", "네", "네놈", "녀석", "년", "년대", "년도", "놈", "누구", "니",
          "다른", "다만", "단", "달", "달러", "당신", "대", "대로", "더구나", "더욱이",
          "데", "도", "동", "되", "두", "두세", "두어","둥", "듯", "듯이", "등","등등", "등지", 
          "따라서", "따름", "따위", "딴", "때문", "또", "또는", "또한",
          "리", "마당", "마련","마리", "만", "만큼", "말", "매", 
          "맨", "명", "몇", "몇몇", "모", "모금", "모든", "무렵", "무슨", "무엇", "뭐",
          "뭣", "미터","및", "바", "바람", "바퀴", "박", "발", "발짝", "번", "벌",
          "법", "별", "본", "부", "분", "뻔", "뿐", "살", "새", "서너", "석", "설",
          "섬", "세", "세기", "셈", "쇤네", "수", "순", 
          "스무", "승", "시", "시간", "식", "씨", "아", "아냐", 
          "아니", "아니야", "아무","아무개","아무런","아아","아이","아이고","아이구","야",
          "약","양","얘","어","어느","어디","어머","언제","에이","엔"
          ,"여기","여느","여러","여러분","여보","여보세요",
          "여지","역시","예","옛","오","오랜","오히려","온","온갖","올",
          "왜냐하면", "왠", "외", "요", "우리", "원", "월", "웬", "위", "음",
          "응", "이", "이거", "이것", "이곳", "이놈", "이래","이런","이런저런","이른바","이리하여",
          "이쪽","일","일대","임마","자","자기","자네","장","저","저것","저기","저놈","저런","저쪽","저편","저희",
          "적","전","점","제","조","주","주년","주일","줄","중","즈음","즉",
          "지경","지난","집","짝","쪽","쯤","차","참","채","척","첫","체","초","총",
          "측","치","큰","킬로미터","타","터","턱","톤","통","투","판","퍼센트",
          "편","평","푼","하기야","하긴","하물며","하지만","한","한두","한편","허허","헌","현",
          "호","혹은","회",
          "하다","있다","되다","회사","단점",
          "장점","가량","가지","각","수","곳",
          "뿐","등","만","시","등","때",
          "나","듯","간","편","외","년",
          "차","것","날","분","중","줄","간","내",
          "뭣","급","점","원","데")

# 한 글자 단어도 남기고(wordLengths), 불용어를 지운 말뭉치를 만든다
make_corpus <- function(text) {
  textProcessor(documents = text, metadata = data,
                wordLengths = c(1, Inf), customstopwords = stopwords_ko)
}
processedcons  <- make_corpus(data$Conspos)
processedpros  <- make_corpus(data$Prospos)
processedtitle <- make_corpus(data$titlepos)

# STM 입력 형식(문서, 어휘, 메타데이터)으로 바꾼다
outcons  <- prepDocuments(processedcons$documents,  processedcons$vocab,  processedcons$meta)
outpros  <- prepDocuments(processedpros$documents,  processedpros$vocab,  processedpros$meta)
outtitle <- prepDocuments(processedtitle$documents, processedtitle$vocab, processedtitle$meta)


# 6. 토픽 수 탐색 ---------------------------------------------------------------
# K = 10, 20, ..., 100에서 응집도, 배타성, 잔차 등을 비교한다
topicN <- seq(10, 100, 10)
storagecons  <- searchK(outcons$documents,  outcons$vocab,  K = topicN)
storagepros  <- searchK(outpros$documents,  outpros$vocab,  K = topicN)
storagetitle <- searchK(outtitle$documents, outtitle$vocab, K = topicN)

storagetitle
plot(storagetitle)


# 7. STM 적합 -------------------------------------------------------------------
# 탐색 결과를 보고 정한 토픽 수로 적합하고, 걸린 시간을 출력한다
fit_stm <- function(out, K) {
  t_start <- Sys.time()
  fit <- stm(documents = out$documents, vocab = out$vocab, data = out$meta,
             K = K,
             max.em.its = 200,        # EM 최대 반복 횟수
             verbose = FALSE,
             init.type = "Spectral",  # 초기값을 스펙트럴 방법으로 정해 결과가 매번 같게 함
             seed = 37)
  print(Sys.time() - t_start)
  fit
}

meta_fit23pros <- fit_stm(outpros, K = 23)  # 장점
meta_fit34cons <- fit_stm(outcons, K = 34)  # 단점


# 8. 대표 단어와 대표 문서 ------------------------------------------------------
# 토픽별 상위 20개 단어
labelTopics(meta_fit23pros, n = 20)  # 장점
labelTopics(meta_fit34cons, n = 20)  # 단점

# 제거된 문서 번호가 없을 때(integer(0)) d[-idx, ]가 빈 자료가 되는 것을 막는다
drop_rows <- function(d, idx) if (length(idx) > 0) d[-idx, ] else d

# 말뭉치를 만들며 빠진 문서를 원자료에서도 빼야 모형의 문서 순서와 원문이 맞는다.
# 토픽별 대표 문서 40개를 찾아 인덱스와 원문을 엑셀 두 시트에 저장한다.
save_thoughts <- function(model, processed, out, texts_col, file) {
  d <- drop_rows(data, processed$docs.removed)  # 형태소, 불용어 처리 후 빈 문서
  d <- drop_rows(d, out$docs.removed)           # prepDocuments에서 빠진 문서
  thoughts <- findThoughts(model, texts = d[[texts_col]], n = 40)

  wb <- createWorkbook()
  addWorksheet(wb, "index")
  addWorksheet(wb, "doc")
  writeDataTable(wb, "index", as.data.frame(thoughts$index))
  writeDataTable(wb, "doc", as.data.frame(thoughts$docs))
  saveWorkbook(wb, file = file, overwrite = TRUE)
}

save_thoughts(meta_fit34cons, processedcons, outcons, "cons", "cons.xlsx")  # 단점
save_thoughts(meta_fit23pros, processedpros, outpros, "pros", "pros.xlsx")  # 장점


# 9. 토픽 비율 그래프 -----------------------------------------------------------
# 토픽별 기대 비율 (x축이 전체 문서에서의 비율)
plot(meta_fit23pros, type = "summary", n = 5, main = "장점 토픽 비율")
plot(meta_fit34cons, type = "summary", n = 5, main = "단점 토픽 비율")
