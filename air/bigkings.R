#################################################################################################################################
#### 필요 라이브러리 로딩
#################################################################################################################################

library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)

#################################################################################################################################
#### bigkinds api 호출
#################################################################################################################################

# API 요청 정보 설정
url <- "https://tools.kinds.or.kr/search/news"
access_key <- readLines("~/projects/keys/key_bigkinds.txt")  # 실제 액세스 키로 대체 필요

bigkinds <- data.frame()
n_corp <- nrow(listed_corps)

init_published_from <- as.Date("2013-01-01T00:00:00Z")
published_from <- init_published_from
published_to   <- as.Date("2023-12-31T00:00:00Z")

for (i in 1:n_corp) {

  # 검색 키워드 설정
  query_keyword <- listed_corps$corp_name[i]
  
  for (j in 1:132) {
    
    published_from <- published_from
    published_to   <- published_from + months(1)
    
    # API에 전송할 요청 본문
    body <- list(
      access_key = access_key,
      argument = list(
        query = query_keyword,
        published_at = list(
          from = published_from,
          until = published_to
        ),
        provider = list(),  # 관심있는 언론사 지정 가능
        category = list("경제"),  # 관심있는 카테고리 지정 가능
        hilight = 200, 
        return_from = 0,
        return_size = 10000,  # 최대 반환 기사 수
        sort = list(date = "desc"),
        fields = list(
          "provider",
          "category"
        )
      )
    )
    
    json_body <- toJSON(body, auto_unbox = TRUE)

    # POST 요청 실행
    response <- POST(url, body = json_body, encode = "json")
    result <- fromJSON(content(response, "text"), flatten = TRUE)
    total_hits <- result$return_object$total_hits
    
    if(total_hits != 0) {
      
      corp_news <- data.frame(corp_code = listed_corps$corp_code[i], corp_name = listed_corps$corp_name[i], query = query_keyword, 
                              published_from = published_from, result)
    } else {
      
      corp_news <- data.frame(corp_code = listed_corps$corp_code[i], corp_name = listed_corps$corp_name[i], query = query_keyword, 
                              published_from = published_from, 
                              result <- NA,
                              return_object.total_hits <- NA,
                              return_object.documents.news_id <- NA,
                              return_object.documents.title <- NA,      
                              return_object.documents.hilight <- NA,
                              return_object.documents.published_at <- NA,
                              return_object.documents.enveloped_at <- NA,
                              return_object.documents.dateline <- NA,   
                              return_object.documents.provider <- NA,  
                              return_object.documents.category <- NA)
      
    }
    
    bigkinds <- bind_rows(bigkinds, corp_news) 
    
    print(paste0("i=", i, ", ", listed_corps$corp_name[i],  ", j = ", j,  ", start_date=", published_from, ", total hits = ", total_hits))
    
    published_from <- published_to
    
    # Sys.sleep(3)
    
  }
  
  published_from <- init_published_from
  
}
