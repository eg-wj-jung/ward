# 패키지
library(dplyr)
library(data.table)
library(Benchmarking)  # dea()

# -----------------------------------------------
# 입력 데이터 가정 (열 이름을 아래와 같이 준비):
# firm_id : 기업식별자
# year    : 연도
# industry: 산업코드(동일 산업 내 비교)
# SALE    : 매출액
# COGS    : 매출원가
# SGA     : 판매비와관리비
# PPE     : 유형자산(=유형자산-토지-건설중인자산)
# INTG    : 무형자산
# -----------------------------------------------

setDT(df)

# (선택) DEA 신뢰도: 산업-연도 표본수 30 미만 집단 제외 (논문 기술 사항)
df <- df %>%
  group_by(industry, year) %>%
  mutate(n_grp = n()) %>%
  ungroup() %>%
  filter(n_grp >= 30)

# 음수/결측 제거 (DEA는 비음수 투입·산출 전제)
df <- df %>%
  filter(!is.na(SALE), !is.na(COGS), !is.na(SGA), !is.na(PPE), !is.na(INTG)) %>%
  mutate(across(c(SALE, COGS, SGA, PPE, INTG), ~pmax(., 0)))

# 산업-연도별 DEA (CCR, 다중투입-단일산출)
# Benchmarking::dea에서 ORIENTATION="out"은 효율값 eff >= 1 (효율적이면 1) → 0~1 스케일로 보고하려면 1/eff로 변환
run_dea_group <- function(dd){
  x <- as.matrix(dd[, c("COGS","SGA","PPE","INTG")])
  y <- as.matrix(dd[, "SALE", drop = FALSE])

  # 규모수익: CRS(CCR), 출력지향(산출확대형)
  fit <- dea(X = x, Y = y, ORIENTATION = "out", RTS = "crs")

  # eff_out >= 1 (효율적이면 1). 논문 표기(0~1, 프론티어=1)에 맞추어 θ = 1/eff로 변환
  dd$eff_out <- fit$eff
  dd$theta   <- 1 / dd$eff_out

  # 경계값 안전처리
  dd$theta[!is.finite(dd$theta)] <- NA_real_
  dd$theta <- pmin(pmax(dd$theta, 0), 1)  # 0~1로 클리핑

  dd
}

df <- df %>%
  group_by(industry, year) %>%
  group_modify(~ run_dea_group(.x)) %>%
  ungroup()

# 결과:
# df$theta : 기업효율성(0~1, 프론티어=1)
# df$eff_out : Benchmarking 기본 출력(≥1, 프론티어=1). 필요 시 참고.
