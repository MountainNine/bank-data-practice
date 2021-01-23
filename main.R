library(ggplot2)
library(dplyr)

df_id <- read.csv("./data/ID.csv")
df_loan <- read.csv("./data/LN.csv")
df_delay <- read.csv("./data/DLQ.csv")
df_cdopn <- read.csv("./data/CDOPN.csv")

df_id$age <- as.integer((2018 - df_id$BTH_YR) / 10)

## 신용거래 경과 기간

count_cdopn <- count(df_cdopn, JOIN_KEY, COM_KEY, CD_OPN_YM)
df_cdopn_new <- count_cdopn %>% group_by(JOIN_KEY) %>% summarize(cd_cnt = min(CD_OPN_YM))
df_result <- merge(df_id, df_cdopn_new, by="JOIN_KEY", all.x = TRUE)
df_result$cd_cnt <- as.integer(2019 - as.integer(df_result$cd_cnt / 100))
rm("df_cdopn_new", "count_cdopn")

## 장기, 단기 연체 발생 여부

df_delay_new <- df_delay %>% group_by(JOIN_KEY, COM_KEY, DLQ_CD_1, DLQ_YM, DLQ_AMT) %>%
  summarize(YM = max(YM))
df_delay_final <- count(df_delay_new, JOIN_KEY)
df_delay_final <- rename(df_delay_final,"delay_cnt"="n")
df_result <- merge(df_result, df_delay, by="")
if(df_delay_new$n<3) {
  df_result$dlq_cnt <- 1
} else if(df_delay_new$n>=3) {
  df_result$dlq_cnt <- 2
}

if(is.na(df_result$dlq_cnt)) {
  df_result$dlq_cnt <- 0
}