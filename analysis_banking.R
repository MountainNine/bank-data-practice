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
df_median_result <- df_id_loan_sum %>% group_by(age) %>%
  summarise(AMT = round(median(SUM_AMT)), TYPE = "median")
df_result <- rbind(df_mean_result, df_median_result)

ggplot(df_result, aes(age, AMT, fill=df_result$TYPE)) + geom_bar(stat='identity', position = 'dodge') +
  geom_text(aes(y=AMT,label=AMT))

# 2. 연체금액 -> 연체횟수에 대한 상관관계

rm("df_id_loan_sum", "df_loan_new", "df_loan_sum", "df_mean_result", "df_median_result", "df_result")

df_delay_new <- df_delay %>% group_by(JOIN_KEY, COM_KEY, SCTR_CD,DLQ_TYPE, DLQ_CD_1, DLQ_CD_2, DLQ_YM,DLQ_AMT) %>%
  summarise(DLQ_COUNT = n())
df_delay_new$DLQ_AMT <- ifelse(df_delay_new$DLQ_AMT < 100000, df_delay_new$DLQ_AMT, NA)

ggplot(df_delay_new, aes(x=DLQ_COUNT, y=DLQ_AMT)) + geom_point()
cor.test(df_delay_new$DLQ_COUNT, df_delay_new$DLQ_AMT)

df_delay_final <- df_delay_new %>% group_by(JOIN_KEY) %>% summarise(DLQ_CNT = sum(DLQ_COUNT), DLQ_AMT = sum(DLQ_AMT))
df_delay_final <- subset(df_delay_final,DLQ_CNT < 100 & DLQ_AMT < 500000)
ggplot(df_delay_final, aes(x=DLQ_CNT, y=DLQ_AMT)) + geom_point()
cor.test(df_delay_final$DLQ_CNT, df_delay_final$DLQ_AMT)

# 3. 카드개설 수 -> 대출 금액의 회귀분석