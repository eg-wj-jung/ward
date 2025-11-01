###########################################################
#### 사업보고서(A001) 다운로드 
###########################################################

# importing the lists of A001 files
list_a001s_rds <- list.files(file.path(data_dir, filing_A001_kospi_rds_dir), full.names = T)

# download A001 files
end_a001 <- length(list_a001s_rds)

for(n_a001s in 1:end_a001){
  
  list_a001s <- readRDS(list_a001s_rds[n_a001s])
  
  list_a001s[, "year"] <- as.numeric(str_extract(list_a001s$report_nm, "[0-9]{4}"))
  
  # list_a001s %>% filter(year == 2024) -> list_a001s # 특정 연도만 추출하고자 할 때 사용
  
  corp_code_a001 <- list_a001s[1, "corp_code"]
  
  a001_kospi_zip_dir <- paste0(data_dir, biz_report_listed_kospi_zip_dir, "corp_code_", corp_code_a001)
  dir_create(a001_kospi_zip_dir)
  
  for(n_a001 in 1:nrow(list_a001s)){
    
    recept_no <- list_a001s[n_a001, "rcept_no"]
    
    request_url_a001 <- paste0("https://opendart.fss.or.kr/api/document.xml?",
                               "&crtfc_key=", key_dart,
                               "&rcept_no=", recept_no)
    
    path_a001 <- paste(a001_kospi_zip_dir, "/", list_a001s[n_a001, "year"], "_", recept_no, ".zip", sep = "")
    
    download.file(url = request_url_a001, destfile = path_a001,
                  # mode = "wb", 
                  quiet = TRUE)
    
    print(paste0("코스피 ", end_a001, "개 중 ", n_a001s, "번째 기업의 ", n_a001, "번째 사업보고서 압축 파일 다운로드 완료"))
    
    Sys.sleep(5 + runif(1) * 2) 
    
  }
  
}

######################################################################
#### 사업보고서 압축파일(zip) 압축 풀기 
######################################################################

list_a001s_kospi_zip <- list.files(file.path(data_dir, biz_report_listed_kospi_zip_dir), recursive = TRUE, full.names = TRUE)

list_a001s <- data.frame(path = list_a001s_kospi_zip, 
                         corp_code = str_extract(list_a001s_kospi_zip, "(?<=corp_code_)[0-9]{1,8}"), 
                         fiscal_year = str_extract_all(list_a001s_kospi_zip, "(?<=/)[0-9]{1,4}", simplify = T)[, 7])

list_a001s <- list_a001s %>% drop_na()
list_a001 <- unique(list_a001s$corp_code)
end_a001s <- length(list_a001)

for(n_corp in 1:end_a001s){
  
  corp_code <- list_a001[n_corp]
  
  list_a001s_sub <- list_a001s[list_a001s$corp_code == corp_code, ]  
  
  n_a001s_sub <- nrow(list_a001s_sub)
  
  for (n_zip in 1:n_a001s_sub) {
    
    path_dir <- paste0(data_dir, biz_report_listed_kospi_xml_dir, "corp_code_", corp_code, "/", list_a001s_sub$fiscal_year[n_zip])
    dir_create(path_dir)
    
    if(file.info(list_a001s_sub[n_zip, "path"])$size > 0) {
      
      unzip(zipfile = list_a001s_sub[n_zip, "path"], exdir = path_dir) 
      
    } 
    
    print(paste0(n_corp, "번째 기업의 ", n_zip, "번째 사업보고서 압축풀기 완료"))
    
  }
  
}



