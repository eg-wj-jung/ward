#################################################################################################################################
#### download A001(KOSDAQ 정기공시 사업보고서) filings as XMLs
#################################################################################################################################

kosdaq <- subset(companies_status, corp_class == "K")
kosdaq$corp_code <- as.numeric(kosdaq$corp_code)

date_begin_a001 <- "20110101"
date_end_a001 <- gsub(pattern = "[^0-9]", replacement = "", x = date)
last_reprt_at_a001 <- "N"
pblntf_ty_a001 <- "A"
pblntf_detail_ty_a001 <- "A001"
page_count_a001 <- 1000
corp_cls <- "Y"
end_kosdaq <- nrow(kosdaq)

for(n_corps in 1:end_kosdaq){
  corp_code_kosdaq <- sprintf(fmt = "%08d", kosdaq[n_corps, "corp_code"])
  
  request_url_kosdaq_a001 <- paste0("https://opendart.fss.or.kr/api/list.xml?",
                                   "&crtfc_key=", key_dart,
                                   "&corp_code=", corp_code_kosdaq,
                                   "&bgn_de=", date_begin_a001,
                                   "&end_de=", date_end_a001,
                                   "&last_reprt_at=", last_reprt_at_a001,
                                   "&pblntf_ty=", pblntf_ty_a001,
                                   "&pblntf_detail_ty=", pblntf_detail_ty_a001,
                                   "&page_count=", page_count_a001,
                                   "&corp_cls=", corp_cls)
  
  kosdaq_a001 <- read_html(request_url_kosdaq_a001, encoding = "UTF-8")
  
  write_xml(kosdaq_a001, 
            paste0(data_dir, filing_A001_kosdaq_xml_dir, corp_code_kosdaq, ".xml"), 
            encoding = "UTF-8")
  print(n_corps)
  Sys.sleep(3 + runif(1) * 2)
}

#################################################################################################################################
#### A001 공시목록 파싱하여 rds로 저장
#################################################################################################################################

list_a001s_kosdaq_xml <- list.files(file.path(data_dir, filing_A001_kosdaq_xml_dir), full.names = T)

for(n_a001 in 1:length(list_a001s_kosdaq_xml)) {
  
  list_a001_kosdaq <- read_html(list_a001s_kosdaq_xml[n_a001], encoding = "UTF-8")
  
  list_a001_kosdaq %>%
    html_nodes(css = "list") %>%
    lapply("xml_child2df") %>% 
    do.call(what = "rbind") -> list_a001_kosdaq_parsed
  
  if(!is.null(list_a001_kosdaq_parsed[1, "rcept_no"])) {
    
    # A001로 인자를 주어도 반기보고서 등이 함께 추출어서 목록에서 사업보고서만 걸러내야 함
    list_a001_kosdaq_parsed <- list_a001_kosdaq_parsed[grep(pattern = "사업보고서", list_a001_kosdaq_parsed$report_nm), ]
    
    if(nrow(list_a001_kosdaq_parsed) >= 1) {
      print(n_a001)
      saveRDS(list_a001_kosdaq_parsed,
              paste0(data_dir, filing_A001_kosdaq_rds_dir, "a001_parsed_", list_a001_kosdaq_parsed[1, "corp_code"], ".rds"))
      # Sys.sleep(time_delay + runif(1) * 2)
    }
    
  }
  
}
