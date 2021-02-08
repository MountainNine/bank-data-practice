library(dplyr)
library(ggplot2)
library(gmodels)

df_id <- read.csv("./data/ID.csv")
df_loan <- read.csv("./data/LN.csv")
df_delay <- read.csv("./data/DLQ.csv")
df_cdopn <- read.csv("./data/CDOPN.csv")

# 1. 연령대별 대출액 평균, 중위수

df_id$age <- as.integer((2018 - df_id$BTH_YR) / 10)
df_loan_new <- df_loan %>% group_by(JOIN_KEY, COM_KEY, SCTR_CD, LN_CD_1, LN_CD_2, LN_YM, LN_AMT) %>%
  summarise(YM = max(YM))
df_loan_sum <- df_loan_new %>% group_by(JOIN_KEY) %>% summarise(SUM_AMT = sum(LN_AMT))
df_id_loan_sum <- merge(df_id, df_loan_sum, by="JOIN_KEY", all.y = TRUE)

df_mean_result <- df_id_loan_sum %>% group_by(age) %>%
  summarise(AMT = round(mean(SUM_AMT)), TYPE = "mean")
df_temp <- df_id_loan_sum %>% group_by(age) %>%
  summarise(AMT = round(median(SUM_AMT)), TYPE = "median")
df_mean_result <- rbind(df_mean_result, df_temp)

ggplot(df_mean_result, aes(age, AMT, fill=df_mean_result$TYPE)) + geom_bar(stat='identity', position = 'dodge') +
  geom_text(aes(y=AMT,label=AMT))

# 2. 연체금액 -> 연체횟수에 대한 상관관계
# 3. 카드개설 수 -> 대출 금액의 회귀분석