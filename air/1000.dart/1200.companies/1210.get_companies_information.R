###################
## 상장회사 선택 ##
###################

corps_code %>% 
  filter(stock_code != " ") -> listed_corps

############################
## 상장기업 개황 다운로드 ##
############################

for(n_corps in 1:nrow(listed_corps)) {
  print(n_corps)
  corp_code_company <- sprintf(fmt = "%08s", listed_corps[n_corps, "corp_code"])
  request_url_company <- paste0("https://opendart.fss.or.kr/api/company.xml?",
                                "&crtfc_key=", key_dart,
                                "&corp_code=", corp_code_company)
  
  company <- read_html(request_url_company, encoding = "UTF-8")
  write_xml(company, 
            paste0(data_dir, company_xml_dir, corp_code_company, ".xml"), 
            encoding = "UTF-8")
  
  Sys.sleep(5 + runif(3))
  
}

companies <- list.files(file.path(data_dir, company_xml_dir), full.names = T)

companies_status <- data.frame()

for(i in 1:length(companies)) {
  raw_xml <- read_html(companies[i], encoding = "UTF-8")
  x_child <- html_children(html_children(raw_xml)) %>% 
    xml_find_all("./*") %>%
    html_text() %>%
    matrix(byrow = TRUE, nrow = 1) %>%
    data.frame()
  companies_status <- rbind(companies_status, x_child)
}

names(companies_status) <- c("status", "message", "corp_code", "corp_name_ko", 
                             "corp_name_eng", "stock_name", "stock_code", 
                             "ceo_name", "corp_class", "jurir_no", "bizr_no", 
                             "addr", "home_url", "ir_url", "phn_no", "fax_no", 
                             "industry_code", "est_date", "fiscal_end")

saveRDS(companies_status, paste0(data_dir, company_rds_dir, "companies_status.rds"))

