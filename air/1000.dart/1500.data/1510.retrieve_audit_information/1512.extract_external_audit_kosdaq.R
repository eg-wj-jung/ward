#################################################
#### 지급별 감사시간(KOSDAQ)
#################################################

list_xml <- list.files(path = file.path(data_dir, biz_report_listed_kosdaq_xml_dir), full.names = TRUE, recursive = TRUE)

##### filtering xml list #####
list_ar_xml <- list_xml[grepl(pattern = "_00760.xml$", x = list_xml)] 

df_list_ar_xml <- data.frame(path = list_ar_xml, 
                             corp_code = str_extract(list_ar_xml, "(?<=corp_code_)[0-9]{1,8}"), 
                             year = str_extract(list_ar_xml, "(?<=[0-9]/)[0-9]{1,4}"), 
                             doc_no = str_extract(list_ar_xml, "(?<=[0-9]/)[0-9]{9,14}"))

##### choose most recent document #####
df_list_ar_xml_split <- split(x = df_list_ar_xml, f = df_list_ar_xml$corp_code)
df_list_ar_xml <- lapply(df_list_ar_xml_split, FUN = "recent_doc")
df_list_ar_xml <- do.call(what = "rbind", args = df_list_ar_xml)
rownames(df_list_ar_xml) <- NULL

##############################################################################
## select time window to process document: 2015년 이후부터 감사시간 등 공시 ##
##############################################################################

### full periods
value_filter_year_min_xml <- 2011
value_filter_year_max_xml <- as.numeric(substr(date, start = 1, stop = 4)) - 1
df_list_ar_xml <- df_list_ar_xml[(df_list_ar_xml$year <= value_filter_year_max_xml) & (df_list_ar_xml$year >= value_filter_year_min_xml), ]

# #### specific period
# df_list_ar_xml %>% filter(year == 2024) -> df_list_ar_xml

corp_list <- unique(df_list_ar_xml$corp_code)

##### set an empty object to store data #####
indv_df_external_audit_kosdaq <- data.frame()
gross_external_audit_kosdaq <- data.frame()

##### global processing #####
start_corp <- 1
end_corp <- length(corp_list)

for(n_corp in 1:end_corp) {
  print(n_corp)
  df_list_ar_xml_sub <- df_list_ar_xml[df_list_ar_xml$corp_code == corp_list[n_corp], ] 
  
  for(n_file in 1:nrow(df_list_ar_xml_sub)) {
    
    ##### document information #####
    xml_doc <- tryCatch(expr = {
      read_html(df_list_ar_xml_sub[n_file, "path"], encoding = "CP949")
    }, error = function(x){
      return(read_html(df_list_ar_xml_sub[n_file, "path"], encoding = "UTF-8"))
    })
    
    xml_doc %>%
      html_nodes(xpath = "//*/company-name") -> corp_info
    
    corp_info %>% 
      html_attr("aregcik") -> corp_code
    
    corp_code <- ifelse(length(corp_code) == 0, NA, corp_code)
    
    corp_info %>% 
      html_text() -> corp_name
    
    corp_name <- ifelse(length(corp_name) == 0, NA, corp_name)
    
    xml_doc %>%
      html_nodes(xpath = "//*/document-name") %>%
      html_attr("acode") -> doc_code
    
    xml_doc %>%
      html_nodes(xpath = "//*/document-name") %>%
      html_text() -> doc_title
    
    doc_title <- ifelse(length(doc_title) == 0, NA, doc_title)
    
    xml_doc %>% 
      html_nodes(xpath = "//*/title[@aassocnote]/..") -> tables
    
    tables %>% 
      html_children() %>%
      html_attr(name = "aassocnote") %>% 
      .[!is.na(.)] -> table_list
    
    xml_doc %>%
      html_nodes(xpath = '//*/tu[@aunit="SUB_PERIODTO"]|//*/tu[@aunit="PERIODTO2"]') %>% 
      html_text() %>% 
      gsub(pattern = "[^0-9]", replacement = "") -> audit_date_end
    
    fiscal_year <- substr(audit_date_end, start = 1, stop = 4)[1]
    fiscal_year <- ifelse(length(fiscal_year) == 0, NA, fiscal_year)
    
    year_end <- substr(audit_date_end, start = 5, stop = 8)[1]    
    year_end <- ifelse(length(year_end) == 0, NA, year_end)
    
    xml_doc %>% 
      html_nodes(xpath = '//*/td[@usermark="F-BT14"]') %>% 
      .[1] %>% 
      html_text() %>% 
      gsub(pattern = "\\n", replacement = "") -> doc_turn
    
    doc_turn <- ifelse(length(doc_turn) == 0, NA, doc_turn)
    
    rcept_no <- str_extract(df_list_ar_xml[n_file, "path"], "(?<=\\/)[0-9]{12,15}")
    
    ##### 감사참여자 구분별 인원수 및 감사시간 #####
    xml_doc %>%
      html_nodes(xpath = '//*/title[@aassocnote="D-0-2-2-0"]/..//title') %>%
      html_text() -> table_name
    
    xml_doc %>%
      html_nodes(xpath = '//*/title[@aassocnote="D-0-2-2-0"]/..//tbody') -> table_sub
    
    table_sub[1] %>%
      html_text() %>%
      gsub(pattern = "\n", replacement = "") -> table_sub_comment
    
    table_sub[2] %>%
      html_nodes(css = "te") %>% 
      html_attr(name = "acode") -> table_sub_names
    
    table_sub[2] %>%
      html_nodes(css = "te") %>% 
      html_text() %>% 
      gsub(pattern = "[^0-9]", replacement = "") %>% 
      ifelse(test = . == "", yes = NA, no = .) %>%
      as.numeric() -> table_sub_data
    
    df_table_hr <- data.frame(var = table_sub_names, value = table_sub_data)
    
    ##### 투입 인력 정보 #####
    if(sum(df_table_hr$var == "NUM_QLT_TH") == 1) {
      
      ##### 외부감사 실시 내용 중 투입 인력 정보 #####
      quality_ctrl <- df_table_hr[df_table_hr$var == "NUM_QLT_TH", "value"]
      
      cpa_pic <- df_table_hr[df_table_hr$var == "NUM_ACT_TH", "value"]
      cpa_regis <- df_table_hr[df_table_hr$var == "NUM_ACR_TH", "value"]
      cpa_intern <- df_table_hr[df_table_hr$var == "NUM_ACP_TH", "value"]
      
      prf_it <- df_table_hr[df_table_hr$var == "NUM_EXP_TH", "value"]
      prf_idus <- df_table_hr[df_table_hr$var == "NUM_COT_TH", "value"]
      
      ##### 외부감사 실시 내용 중 투입 시간 정보(분/반기) ######
      quality_ctrl_time_periodic <- df_table_hr[df_table_hr$var == "TMA_QLT_TH", "value"]
      
      cpa_pic_time_periodic <- df_table_hr[df_table_hr$var == "TMA_ACT_TH", "value"]
      cpa_regis_time_periodic <- df_table_hr[df_table_hr$var == "TMA_ACR_TH", "value"]
      cpa_intern_time_periodic <- df_table_hr[df_table_hr$var == "TMA_ACP_TH", "value"]
      
      prf_it_time_periodic <- df_table_hr[df_table_hr$var == "TMA_EXP_TH", "value"]
      prf_idus_time_periodic <- df_table_hr[df_table_hr$var == "TMA_COT_TH", "value"]
      
      ##### 외부감사 실시 내용 중 투입 시간 정보(기말) #####
      quality_ctrl_time_yearend <- df_table_hr[df_table_hr$var == "TMY_QLT_TH", "value"]
      
      cpa_pic_time_yearend <- df_table_hr[df_table_hr$var == "TMY_ACT_TH", "value"]
      cpa_regis_time_yearend <- df_table_hr[df_table_hr$var == "TMY_ACR_TH", "value"]
      cpa_intern_time_yearend <- df_table_hr[df_table_hr$var == "TMY_ACP_TH", "value"]
      
      prf_it_time_yearend <- df_table_hr[df_table_hr$var == "TMY_EXP_TH", "value"]
      prf_idus_time_yearend <- df_table_hr[df_table_hr$var == "TMY_COT_TH", "value"]
      
    } else {
      
      ##### 외부감사 실시 내용 중 투입 인력 정보 #####
      quality_ctrl <- df_table_hr[df_table_hr$var == "NUM_QLT", "value"]
      
      cpa_pic <- df_table_hr[df_table_hr$var == "NUM_ACT", "value"]
      cpa_regis <- df_table_hr[df_table_hr$var == "NUM_ACR", "value"]
      cpa_intern <- df_table_hr[df_table_hr$var == "NUM_ACP", "value"]
      
      prf_it <- df_table_hr[df_table_hr$var == "NUM_EXP", "value"]
      prf_idus <- NA
      
      ##### 외부감사 실시 내용 중 투입 시간 정보(분/반기) ######
      quality_ctrl_time_periodic <- df_table_hr[df_table_hr$var == "TMA_QLT", "value"]
      
      cpa_pic_time_periodic <- df_table_hr[df_table_hr$var == "TMA_ACT", "value"]
      cpa_regis_time_periodic <- df_table_hr[df_table_hr$var == "TMA_ACR", "value"]
      cpa_intern_time_periodic <- df_table_hr[df_table_hr$var == "TMA_ACP", "value"]
      
      prf_it_time_periodic <- df_table_hr[df_table_hr$var == "TMA_EXP", "value"]
      prf_idus_time_periodic <- NA
      
      ##### 외부감사 실시 내용 중 투입 시간 정보(기말) #####
      quality_ctrl_time_yearend <- df_table_hr[df_table_hr$var == "TMY_QLT", "value"]
      
      cpa_pic_time_yearend <- df_table_hr[df_table_hr$var == "TMY_ACT", "value"]
      cpa_regis_time_yearend <- df_table_hr[df_table_hr$var == "TMY_ACR", "value"]
      cpa_intern_time_yearend <- df_table_hr[df_table_hr$var == "TMY_ACP", "value"]
      
      prf_it_time_yearend <- df_table_hr[df_table_hr$var == "TMY_EXP", "value"]
      prf_idus_time_yearend <- NA
      
    }
    
    quality_ctrl <- ifelse(length(quality_ctrl) == 0, NA, quality_ctrl)
    cpa_pic <- ifelse(length(cpa_pic) == 0, NA, cpa_pic)
    cpa_regis <- ifelse(length(cpa_regis) == 0, NA, cpa_regis)
    cpa_intern <- ifelse(length(cpa_intern) == 0, NA, cpa_intern)
    prf_it <- ifelse(length(prf_it) == 0, NA, prf_it)
    prf_idus <- ifelse(length(prf_idus) == 0, NA, prf_idus)
    quality_ctrl_time_periodic <- ifelse(length(quality_ctrl_time_periodic) == 0, NA, quality_ctrl_time_periodic)
    cpa_pic_time_periodic <- ifelse(length(cpa_pic_time_periodic) == 0, NA, cpa_pic_time_periodic)
    cpa_regis_time_periodic <- ifelse(length(cpa_regis_time_periodic) == 0, NA, cpa_regis_time_periodic)
    cpa_intern_time_periodic <- ifelse(length(cpa_intern_time_periodic) == 0, NA, cpa_intern_time_periodic)
    prf_it_time_periodic <- ifelse(length(prf_it_time_periodic) == 0, NA, prf_it_time_periodic)
    prf_idus_time_periodic <- ifelse(length(prf_idus_time_periodic) == 0, NA, prf_idus_time_periodic)
    quality_ctrl_time_yearend <- ifelse(length(quality_ctrl_time_yearend) == 0, NA, quality_ctrl_time_yearend)
    cpa_pic_time_yearend <- ifelse(length(cpa_pic_time_yearend) == 0, NA, cpa_pic_time_yearend)
    cpa_regis_time_yearend <- ifelse(length(cpa_regis_time_yearend) == 0, NA, cpa_regis_time_yearend)
    cpa_intern_time_yearend <- ifelse(length(cpa_intern_time_yearend) == 0, NA, cpa_intern_time_yearend)
    prf_it_time_yearend <- ifelse(length(prf_it_time_yearend) == 0, NA, prf_it_time_yearend)
    prf_idus_time_yearend <- ifelse(length(prf_idus_time_yearend) == 0, NA, prf_idus_time_yearend)
    
    ##### 외부 감사 실시 내용(집합) #####
    df_external_audit <- data.frame(corp_name = corp_name,
                                    corp_code = corp_code, 
                                    fiscal_year = fiscal_year,
                                    year_end = year_end, 
                                    fiscal_turn = doc_turn,
                                    quality_ctrl = quality_ctrl,
                                    cpa_pic = cpa_pic,
                                    cpa_regis = cpa_regis,
                                    cpa_intern = cpa_intern,
                                    prf_it = prf_it,
                                    prf_idus = prf_idus,
                                    quality_ctrl_time_periodic = quality_ctrl_time_periodic,
                                    cpa_pic_time_periodic = cpa_pic_time_periodic,
                                    cpa_regis_time_periodic = cpa_regis_time_periodic,
                                    cpa_intern_time_periodic = cpa_intern_time_periodic,
                                    prf_it_time_periodic = prf_it_time_periodic,
                                    prf_idus_time_periodic = prf_idus_time_periodic,
                                    quality_ctrl_time_yearend = quality_ctrl_time_yearend,
                                    cpa_pic_time_yearend = cpa_pic_time_yearend,
                                    cpa_regis_time_yearend = cpa_regis_time_yearend,
                                    cpa_intern_time_yearend = cpa_intern_time_yearend,
                                    prf_it_time_yearend = prf_it_time_yearend,
                                    prf_idus_time_yearend = prf_idus_time_yearend
    )
    
    indv_df_external_audit_kosdaq <- bind_rows(indv_df_external_audit_kosdaq, df_external_audit)
    df_external_audit <- data.frame()
  }
  
  gross_external_audit_kosdaq <- bind_rows(gross_external_audit_kosdaq, indv_df_external_audit_kosdaq)
  indv_df_external_audit_kosdaq <- data.frame()
}

#### write for rds #####
saveRDS(gross_external_audit_kosdaq, "~/data/raw/10000.dart/15000.data/ex_audit/external_audit_kosdaq.rds")
