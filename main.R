library(ggplot2)
library(dplyr)
library(lubridate)

df_id <- read.csv("./data/ID.csv")
df_loan <- read.csv("./data/LN.csv")
df_delay <- read.csv("./data/DLQ.csv")
df_cdopn <- read.csv("./data/CDOPN.csv")

df_id$age <- as.integer((2018 - df_id$BTH_YR) / 10)

get_month_diff <- function(datetime_1, datetime_2) {
  return((year(datetime_1) * 12 + month(datetime_1)) - (year(datetime_2) * 12 + month(datetime_2)))
}

get_delay_score <- function(delay_new) {
  result_score <- -100
  current_date <- as.Date('20181201', format = '%Y%m%d')
  dlq_amt <- delay_new$DLQ_AMT
  end_ym <- delay_new$YM
  delay_cnt <- delay_new$delay_cnt
  result_score <- ifelse(delay_cnt < 4, result_score + (-100 * delay_cnt + (-0.001 * dlq_amt)),
                         result_score + (-400 +
                           (-10 * (delay_cnt - 4)) +
                           (-0.001 * dlq_amt)))

  diff_month <- get_month_diff(current_date, end_ym)

  result_score <- ifelse(end_ym < current_date,
                         ifelse(delay_cnt < 4, result_score + 100 + diff_month * 10, result_score + 10 + diff_month * 10),
                         result_score)

  return(result_score)
}

## 단기연체 발생(-100)
## 장기연체 발생(-100)
## 연체 진행 일수 경과(월에 -100/-10)
## 연체 해제(+100)
## 연체 해제 일수 경과(월에 +10)

## 신용거래 경과 기간

count_cdopn <- count(df_cdopn, JOIN_KEY, COM_KEY, CD_OPN_YM)
df_cdopn_new <- count_cdopn %>%
  group_by(JOIN_KEY) %>%
  summarize(cd_cnt = min(CD_OPN_YM))
df_result <- merge(df_id, df_cdopn_new, by = "JOIN_KEY", all.x = TRUE)
df_result$cd_cnt <- as.integer(2019 - as.integer(df_result$cd_cnt / 100))
rm("df_cdopn_new", "count_cdopn")

## 장기, 단기 연체 발생 여부

df_delay_new <- df_delay %>%
  group_by(JOIN_KEY, COM_KEY, DLQ_CD_1, DLQ_YM, DLQ_AMT) %>%
  summarise(YM = max(YM))
df_delay_new$YM <- paste0(as.character(df_delay_new$YM), "01")
df_delay_new$YM <- as.Date(df_delay_new$YM, format = "%Y%m%d")
df_delay_new$DLQ_YM <- paste0(as.character(df_delay_new$DLQ_YM), "01")
df_delay_new$DLQ_YM <- as.Date(df_delay_new$DLQ_YM, format = "%Y%m%d")
df_delay_new$delay_cnt <- get_month_diff(df_delay_new$YM, df_delay_new$DLQ_YM) + 1

df_delay_new$dlq_score <- get_delay_score(df_delay_new)
df_delay_final <- df_delay_new %>%
  group_by(JOIN_KEY) %>%
  summarise(dlq_score = sum(dlq_score))
df_result <- merge(df_result, df_delay_final, by = "JOIN_KEY", all.x = TRUE)
df_result[is.na(df_result$dlq_score),]$dlq_score <- 0

rm("df_delay_new", "df_delay_final")