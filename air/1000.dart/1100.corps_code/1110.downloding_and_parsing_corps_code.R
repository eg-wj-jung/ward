############################################################
## download and unzip the corporate unique code 
############################################################

## download unique corporate code
corps_code_zip <- paste0(data_dir, corps_code_zip_dir, "corps_code.zip")
request_url_corps_code <- paste0("https://opendart.fss.or.kr/api/corpCode.xml?&crtfc_key=", key_dart)
download.file(url = request_url_corps_code, 
              destfile = corps_code_zip,
              # mode = "wb", # 윈도우즈의 경우 mode 인자 사용
              quiet = TRUE)

## unzip
unzip(zipfile = corps_code_zip, exdir = file.path(data_dir, corps_code_unzip_dir))

## parsing xml to csv
corps_code_xml <- paste0(data_dir, corps_code_unzip_dir, "CORPCODE.xml")

read_xml(corps_code_xml) %>%
  html_nodes(css = "list") %>%
  lapply("xml_child2df") %>%
  do.call(what = "rbind") -> corps_code

for (i in 1:nrow(corps_code)) {
  if (corps_code$stock_code[i] == " ") {
    corps_code$stock_code[i] <- NA  
  }
}

corps_code_rds <- paste0(data_dir, corps_code_parsed_dir, "corps_code.rds")
saveRDS(corps_code, corps_code_rds)

