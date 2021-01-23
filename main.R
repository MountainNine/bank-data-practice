library(ggplot2)
library(dplyr)
library(lubridate)

df_id <- read.csv("./data/ID.csv")
df_loan <- read.csv("./data/LN.csv")
df_delay <- read.csv("./data/DLQ.csv")
df_cdopn <- read.csv("./data/CDOPN.csv")

df_id$age <- as.integer((2018 - df_id$BTH_YR) / 10)

get_delay_score <- function(end_ym, dlq_amt, delay_cnt) {
  result_score <- -100
  current_date <- as.Date('201812', format = '%Y%m')
  if (delay_cnt < 4) {
    result_score <- result_score + (-100 * delay_cnt + (-0.001 * dlq_amt))
  } else {
    result_score <- result_score + (-400 +
      (-10 * (delay_cnt - 4)) +
      (-0.001 * dlq_amt))
  }

  if (end_ym < current_date & delay_cnt < 4) {
    result_score <- result_score +
      100 +
      difftime(end_ym, current_date) / 30 * 10
  } else if (end_ym < as.Date("2018-12-31") & delay_cnt >= 4) {
    result_score <- result_score +
      10 +
      difftime(end_ym, current_date) / 30 * 10
  }
  return(result_score)
}

## 단기연체 발생(-100)
## 장기연체 발생(-100)
## 연체 진행 일수 경과(월에 -100/-10)
## 연체 해제(+100)
## 연체 해제 일수 경과(월에 +10)
}

## 신용거래 경과 기간

count_cdopn <- count(df_cdopn, JOIN_KEY, COM_KEY, CD_OPN_YM)
df_cdopn_new <- count_cdopn %>% group_by(JOIN_KEY) %>% summarize(cd_cnt = min(CD_OPN_YM))
df_result <- merge(df_id, df_cdopn_new, by="JOIN_KEY", all.x = TRUE)
df_result$cd_cnt <- as.integer(2019 - as.integer(df_result$cd_cnt / 100))
rm("df_cdopn_new", "count_cdopn")

## 장기, 단기 연체 발생 여부

df_delay_new <- df_delay %>% group_by(JOIN_KEY, COM_KEY, DLQ_CD_1, DLQ_YM, DLQ_AMT) %>%
summarise(YM = max(YM), delay_cnt = n())
df_delay_final <- count(df_delay_new, JOIN_KEY)
df_delay_final <- rename(df_delay_final, "delay_cnt"="n")
df_result <- merge(df_result, df_delay, by="")
if(df_delay_new$n<3){
df_result$dlq_cnt <- 1
} else if(df_delay_new$n>=3){
df_result$dlq_cnt <- 2
}

if(is.na(df_result$dlq_cnt)){
df_result$dlq_cnt<-0
}