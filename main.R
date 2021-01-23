library(ggplot2)
library(dplyr)

df_id <- read.csv("./data/ID.csv")
df_loan <- read.csv("./data/LN.csv")
df_delay <- read.csv("./data/DLQ.csv")
df_cdopn <- read.csv("./data/CDOPN.csv")

# df_id$age <- as.integer((2018 - df_id$BTH_YR) / 10)

## 신용거래 경과 기간

count_cdopn <- count(df_cdopn, JOIN_KEY, COM_KEY, CD_OPN_YM)
df_cdopn <- count_cdopn %>% group_by(JOIN_KEY) %>% summarise(cd_amt = min(CD_OPN_YM))
df_id <- merge(df_id, df_cdopn, by="JOIN_KEY", all.x = TRUE)