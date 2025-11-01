################################################
## Setting Global Environments
################################################

# DART 키 불러오기
key_dart <- readLines("~/projects/keys/key_dart.txt", warn = FALSE)


# 라이브러리 로딩
library(rvest)
library(tidyverse)
library(jsonlite)
library(xml2)
library(fs)
library(stringr)
library(writexl)

# 날짜 지정
date <- Sys.Date()
date_lag <- date -1


# 기본 디렉토리 설정
code_dir <- "~/projects/ward/"
data_dir <- "~/data/raw/999999.etc/"

# DART 기준 고유기업코드 다운로드 및 처리 위한 경로 설정
corps_code_zip_dir        <- "10000.dart/11000.corps_code/11100.zip/"
corps_code_unzip_dir      <- "10000.dart/11000.corps_code/11200.xml/"
corps_code_parsed_dir     <- "10000.dart/11000.corps_code/11300.rds/"

dir_create(data_dir, corps_code_zip_dir)
dir_create(data_dir, corps_code_unzip_dir)
dir_create(data_dir, corps_code_parsed_dir)

# 상장기업개황 데이터 다운로드 및 처리 위한 경로 결정
company_xml_dir         <- "10000.dart/12000.company/12100.listed/12110.xml/"
company_rds_dir         <- "10000.dart/12000.company/12100.listed/12120.rds/"

dir_create(data_dir, company_xml_dir)
dir_create(data_dir, company_rds_dir)

# filing for A001
filing_A001_kospi_xml_dir <- "10000.dart/13000.filings/13100.A001/13110.kospi/13111.xml/"
filing_A001_kospi_rds_dir <- "10000.dart/13000.filings/13100.A001/13110.kospi/13112.rds/"

dir_create(data_dir, filing_A001_kospi_xml_dir)
dir_create(data_dir, filing_A001_kospi_rds_dir)

filing_A001_kosdaq_xml_dir <- "10000.dart/13000.filings/13100.A001/13120.kosdaq/13121.xml/"
filing_A001_kosdaq_rds_dir <- "10000.dart/13000.filings/13100.A001/13120.kosdaq/13122.rds/"

dir_create(data_dir, filing_A001_kosdaq_xml_dir)
dir_create(data_dir, filing_A001_kosdaq_rds_dir)

# biz report
biz_report_listed_kospi_zip_dir <- "10000.dart/14000.reports/14100.biz_reports_listed/14110.kospi/14111.zip/"
biz_report_listed_kospi_xml_dir <- "10000.dart/14000.reports/14100.biz_reports_listed/14110.kospi/14112.xml/"

dir_create(data_dir, biz_report_listed_kospi_zip_dir)
dir_create(data_dir, biz_report_listed_kospi_xml_dir)

biz_report_listed_kosdaq_zip_dir <- "10000.dart/14000.reports/14100.biz_reports_listed/14120.kosdaq/14121.zip/"
biz_report_listed_kosdaq_xml_dir <- "10000.dart/14000.reports/14100.biz_reports_listed/14120.kosdaq/14122.xml/"

dir_create(data_dir, biz_report_listed_kosdaq_zip_dir)
dir_create(data_dir, biz_report_listed_kosdaq_xml_dir)

# A001
ex_audit_data_rds_kospi   <- file.path("10000.dart/15000.data/")
ex_audit_data_rds_kosdaq  <- file.path("10000.dart/15000.data/")

dir_create(data_dir, ex_audit_data_rds_kospi)
dir_create(data_dir, ex_audit_data_rds_kosdaq)

# creating xml_child2df function to import and process xml
xml_child2df <- function(x){
  x_child <- html_children(x)
  x_df <- data.frame(matrix(html_text(x_child), byrow = TRUE, nrow = 1))
  colnames(x_df) <- html_name(x_child)
  return(x_df)
}

# select most recent report
recent_doc <- function(x){
  aggregate(data = x, . ~ year, FUN = "max")   
}

latest_doc <- function(x){
  aggregate(data = x, . ~ rcept_dt, FUN = "max")   
}

earliest_doc <- function(x){
  aggregate(data = x, . ~ year, FUN = "min")   
}


