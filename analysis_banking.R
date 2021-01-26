library(dplyr)
library(ggplot2)

df_id <- read.csv("./data/ID.csv")
df_loan <- read.csv("./data/LN.csv")
df_delay <- read.csv("./data/DLQ.csv")
df_cdopn <- read.csv("./data/CDOPN.csv")

# 1. 연령대별 주요 대출상품 교차분석
# 2. 연체금액 -> 연체횟수에 대한 상관관계
# 3. 카드개설 수 -> 대출 금액의 회귀분석