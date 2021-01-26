library(dplyr)
library(ggplot2)
library(gmodels)

df_id <- read.csv("./data/ID.csv")
df_loan <- read.csv("./data/LN.csv")
df_delay <- read.csv("./data/DLQ.csv")
df_cdopn <- read.csv("./data/CDOPN.csv")

# 1. 연령대별 주요 대출상품 교차분석

df_id$age <- as.integer((2018 - df_id$BTH_YR) / 10)
df_loan_new <- df_loan %>% group_by(JOIN_KEY, COM_KEY, SCTR_CD, LN_CD_1, LN_CD_2, LN_YM, LN_AMT) %>%
  summarise(YM = max(YM))

df_id_loan <- merge(df_id, df_loan_new, by="JOIN_KEY")
x <- df_id_loan$age
y <- paste0(df_id_loan$LN_CD_1, df_id_loan$LN_CD_2)
y <- ifelse(nchar(y) < 5, paste0(y,"00"),y)
result <- data.frame(AGE = x, LN_CODE = y)
table_result <- CrossTable(result$AGE, result$LN_CODE)
df_result <- as.data.frame(table_result$prop.row)
df_result <- df_result[order(df_result$x, -df_result$Freq),]
final_result <- df_result %>% group_by(x) %>% filter(row_number()==c(1,2))
# 2. 연체금액 -> 연체횟수에 대한 상관관계
# 3. 카드개설 수 -> 대출 금액의 회귀분석